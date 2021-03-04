package zio.sql

import java.io.IOException
import java.sql._

import zio.stm._
import zio._
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
   * A live layer for `ConnectionPool` that creates a JDBC connection pool
   * from the specified connection pool settings.
   */
  val live: ZLayer[Has[ConnectionPoolConfig] with Blocking with Clock, IOException, Has[ConnectionPool]] =
    (for {
      config    <- ZManaged.service[ConnectionPoolConfig]
      blocking  <- ZManaged.service[Blocking.Service]
      clock     <- ZManaged.service[Clock.Service]
      queue     <- TQueue.bounded[TPromise[Nothing, ResettableConnection]](config.queueCapacity).commit.toManaged_
      available <- TRef.make(List.empty[ResettableConnection]).commit.toManaged_
      pool       = ConnectionPoolLive(queue, available, config, clock, blocking)
      _         <- pool.initialize.toManaged_
      _         <- ZManaged.finalizer(pool.close.orDie)
    } yield pool).toLayer
}

/**
 * A live concurrent connection pool.
 *
 * Improvements to make:
 *
 *  - A connection may die. If so, it should be reacquired.
 *  - Someone may try to use a connection forever. If so, we should
 *    take it away from them.
 */
final case class ConnectionPoolLive(
  queue: TQueue[TPromise[Nothing, ResettableConnection]],
  available: TRef[List[ResettableConnection]],
  config: ConnectionPoolConfig,
  clock: Clock.Service,
  blocking: Blocking.Service
) extends ConnectionPool {

  /**
   * Adds a fresh connection to the connection pool.
   */
  val addFreshConnection: IO[IOException, ResettableConnection] = {
    val makeConnection = blocking.effectBlocking {
      val connection = DriverManager.getConnection(config.url, config.properties)

      val autoCommit  = connection.getAutoCommit()
      val catalog     = connection.getCatalog()
      val clientInfo  = connection.getClientInfo()
      val holdability = connection.getHoldability()
      val schema      = connection.getSchema()
      val isolation   = connection.getTransactionIsolation()

      val restore: Connection => Unit = connection => {
        if (connection.getAutoCommit() != autoCommit) connection.setAutoCommit(autoCommit)
        if (connection.getCatalog() ne catalog) connection.setCatalog(catalog)
        if (connection.getClientInfo() ne clientInfo) connection.setClientInfo(clientInfo)
        if (connection.getHoldability() != holdability) connection.setHoldability(holdability)
        if (connection.getSchema() != schema) connection.setSchema(schema)
        if (connection.getTransactionIsolation() != isolation) connection.setTransactionIsolation(isolation)
      }

      new ResettableConnection(connection, restore)
    }.refineToOrDie[IOException]

    for {
      connection <- makeConnection.retry(config.retryPolicy).provide(Has(clock))
      _          <- available.update(connection :: _).commit
    } yield connection
  }

  /**
   * Closes the connection pool, terminating each connection in parallel.
   */
  val close: IO[IOException, Any] =
    ZIO.uninterruptible {
      available.get.commit.flatMap { all =>
        blocking.blocking {
          ZIO.foreachPar(all) { connection =>
            ZIO(connection.connection.close()).refineToOrDie[IOException]
          }
        }
      }
    }

  def connection: Managed[Exception, Connection] =
    ZManaged
      .make(tryTake.commit.flatMap {
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
      .flatMap(rc => rc.reset.toManaged_.as(rc.connection))

  /**
   * Initializes the connection pool.
   */
  val initialize: IO[IOException, Unit] =
    ZIO.uninterruptible {
      ZIO
        .foreachPar_(1 to config.poolSize) { _ =>
          addFreshConnection
        }
        .unit
    }

  private def release(connection: ResettableConnection): UIO[Any] =
    ZIO.uninterruptible {
      tryRelease(connection).commit.flatMap {
        case Some(promise) => promise.succeed(connection).commit
        case None          => UIO.unit
      }
    }

  private def tryRelease(
    connection: ResettableConnection
  ): STM[Nothing, Option[TPromise[Nothing, ResettableConnection]]] =
    for {
      empty  <- queue.isEmpty
      result <- if (empty) available.update(connection :: _) *> STM.none
                else queue.take.map(Some(_))
    } yield result

  private val tryTake: STM[Nothing, Either[TPromise[Nothing, ResettableConnection], ResettableConnection]] =
    for {
      headOption <- available.get.map(_.headOption)
      either     <- headOption match {
                      case None =>
                        for {
                          promise <- TPromise.make[Nothing, ResettableConnection]
                          _       <- queue.offer(promise)
                        } yield Left(promise)

                      case Some(connection) =>
                        available.update(_.tail) *> ZSTM.succeed(Right(connection))
                    }
    } yield either
}

private[sql] final class ResettableConnection(val connection: Connection, resetter: Connection => Unit) {
  def reset: UIO[Any] = UIO(resetter(connection))
}
