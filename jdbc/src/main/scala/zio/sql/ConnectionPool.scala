package zio.sql

import java.io.IOException
import java.sql._

import zio.stm._
import zio._
import zio.duration._
import zio.blocking._
import zio.clock._

trait ConnectionPool {

  /**
   * Retrieves a JDBC [[java.sql.Connection]] as a [[zio.Managed]] resource.
   * The managed resource will safely acquire and release the connection, and
   * may be interrupted or timed out if necessary.
   */
  def connection: Managed[Exception, Connection]
}
object ConnectionPool {

  /**
   * Configuration information for the connection pool.
   *
   * @param url            The JDBC connection string.
   * @param properties     JDBC connection properties (username / password could go here).
   * @param poolSize       The size of the pool.
   * @param queueCapacity  The capacity of the queue for connections. When this size is reached, back pressure will block attempts to add more.
   * @param retryPolicy    The retry policy to use when acquiring connections.
   */
  final case class Config(
    url: String,
    properties: java.util.Properties,
    poolSize: Int = 10,
    queueCapacity: Int = 1000,
    retryPolicy: Schedule[Any, Exception, Any] = Schedule.recurs(20) && Schedule.exponential(10.millis)
  )

  val live: ZLayer[Has[Config] with Blocking with Clock, IOException, Has[ConnectionPool]] =
    (for {
      config    <- ZManaged.service[Config]
      blocking  <- ZManaged.service[Blocking.Service]
      clock     <- ZManaged.service[Clock.Service]
      queue     <- TQueue.bounded[TPromise[Nothing, Connection]](config.queueCapacity).commit.toManaged_
      available <- TRef.make(List.empty[Connection]).commit.toManaged_
      taken     <- TRef.make(List.empty[Connection]).commit.toManaged_
      pool       = ConnectionPoolLive(queue, available, taken, config, clock, blocking)
      _         <- pool.initialize.toManaged_
      _         <- ZManaged.finalizer(pool.close.orDie)
    } yield pool).toLayer
}

final case class ConnectionPoolLive(
  queue: TQueue[TPromise[Nothing, Connection]],
  available: TRef[List[Connection]],
  taken: TRef[List[Connection]],
  config: ConnectionPool.Config,
  clock: Clock.Service,
  blocking: Blocking.Service
) extends ConnectionPool {

  /**
   * Closes the connection pool, terminating each connection in parallel.
   */
  val close: IO[IOException, Any] =
    ZIO.uninterruptible {
      available.get.commit.zipWith(taken.get.commit)(_ ++ _).flatMap { all =>
        blocking.blocking {
          ZIO.foreachPar(all) { connection =>
            ZIO(connection.close()).refineToOrDie[IOException]
          }
        }
      }
    }

  def connection: Managed[Exception, Connection] =
    ZManaged.make(tryTake.commit.flatMap {
      case Left(promise) =>
        ZIO.interruptible(promise.await.commit).onInterrupt {
          promise.poll.flatMap {
            case Some(Right(connection)) =>
              ZSTM.succeed(release(connection))

            case _ => ZSTM.succeed(ZIO.unit)
          }.commit.flatten
        }

      case Right(connection) =>
        ZIO.succeed(connection)
    })(release(_))

  /**
   * Initializes the connection pool.
   */
  val initialize: IO[IOException, Unit] = {
    val makeConnection = blocking.effectBlocking {
      DriverManager.getConnection(config.url, config.properties)
    }.refineToOrDie[IOException]

    ZIO
      .foreachPar_(1 to config.poolSize) { _ =>
        ZIO.uninterruptible {
          for {
            connection <- makeConnection.retry(config.retryPolicy).provide(Has(clock))
            _          <- available.update(connection :: _).commit
          } yield connection
        }
      }
      .unit
  }

  private def release(connection: Connection): UIO[Any] =
    ZIO.uninterruptible {
      tryRelease(connection).commit.flatMap {
        case Some(promise) => promise.succeed(connection).commit
        case None          => UIO.unit
      }
    }

  private def tryRelease(connection: Connection): STM[Nothing, Option[TPromise[Nothing, Connection]]] =
    for {
      empty  <- queue.isEmpty
      result <- if (empty) available.update(connection :: _) *> STM.none
                else queue.take.map(Some(_))
    } yield result

  private val tryTake: STM[Nothing, Either[TPromise[Nothing, Connection], Connection]] =
    for {
      headOption <- available.get.map(_.headOption)
      either     <- headOption match {
                      case None =>
                        for {
                          promise <- TPromise.make[Nothing, Connection]
                          _       <- queue.offer(promise)
                        } yield Left(promise)

                      case Some(connection) =>
                        available.update(_.tail) *> ZSTM.succeed(Right(connection))
                    }
    } yield either
}
