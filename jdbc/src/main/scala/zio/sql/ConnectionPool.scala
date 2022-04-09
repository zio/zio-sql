package zio.sql

import java.io.IOException
import java.sql._

import zio.stm._
import zio._

import zio.sql.ConnectionPool.QueueItem

trait ConnectionPool {

  /**
   * Retrieves a JDBC java.sql.Connection as a [[ZIO[Scope, Exception, Connection]]] resource.
   * The managed resource will safely acquire and release the connection, and
   * may be interrupted or timed out if necessary.
   */
  def connection: ZIO[Scope, Exception, Connection]
}
object ConnectionPool {

  case class QueueItem(promise: TPromise[Nothing, ResettableConnection], interrupted: TRef[Boolean])

  /**
   * A live layer for `ConnectionPool` that creates a JDBC connection pool
   * from the specified connection pool settings.
   */
  val live: ZLayer[ConnectionPoolConfig, IOException, ConnectionPool] =
    ZLayer.scoped {
      for {
        config    <- ZIO.service[ConnectionPoolConfig]
        queue     <- TQueue.bounded[QueueItem](config.queueCapacity).commit
        available <- TRef.make(List.empty[ResettableConnection]).commit
        pool       = ConnectionPoolLive(queue, available, config)
        _         <- pool.initialize
        _         <- ZIO.addFinalizer(pool.close.orDie)
      } yield pool
    }
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
  queue: TQueue[QueueItem],
  available: TRef[List[ResettableConnection]],
  config: ConnectionPoolConfig
) extends ConnectionPool {

  /**
   * Adds a fresh connection to the connection pool.
   */
  val addFreshConnection: IO[IOException, ResettableConnection] = {
    val makeConnection = ZIO.attemptBlocking {
      val connection = DriverManager.getConnection(config.url, config.properties)

      val autoCommit  = config.autoCommit
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
      connection <- makeConnection.retry(config.retryPolicy)
      _          <- available.update(connection :: _).commit
    } yield connection
  }

  /**
   * Closes the connection pool, terminating each connection in parallel.
   */
  val close: IO[IOException, Any] =
    ZIO.uninterruptible {
      available.get.commit.flatMap { all =>
        ZIO.foreachPar(all) { connection =>
          ZIO.attemptBlocking(connection.connection.close()).refineToOrDie[IOException]
        }
      }
    }

  def connection: ZIO[Scope, Exception, Connection] =
    ZIO
      .acquireRelease(tryTake.commit.flatMap {
        case Left(queueItem) =>
          ZIO.interruptible(queueItem.promise.await.commit).onInterrupt {
            (for {
              res <- queueItem.promise.poll
              _   <- res match {
                       case Some(Right(connection)) =>
                         ZSTM.succeed(release(connection))
                       case _                       =>
                         queueItem.interrupted.set(true)
                     }
            } yield ()).commit
          }

        case Right(connection) =>
          ZIO.succeed(connection)
      })(release(_))
      .flatMap(rc => rc.reset.as(rc.connection))

  /**
   * Initializes the connection pool.
   */
  val initialize: IO[IOException, Unit] =
    ZIO.uninterruptible {
      ZIO
        .foreachParDiscard(1 to config.poolSize) { _ =>
          addFreshConnection
        }
        .unit
    }

  private def release(connection: ResettableConnection): UIO[Any] =
    ZIO.uninterruptible {
      tryRelease(connection).commit.flatMap {
        case Some(handle) =>
          handle.interrupted.get.tap { interrupted =>
            ZSTM.when(!interrupted)(handle.promise.succeed(connection))
          }.commit.flatMap { interrupted =>
            ZIO.when(interrupted)(release(connection))
          }
        case None         => UIO.unit
      }
    }

  private def tryRelease(
    connection: ResettableConnection
  ): STM[Nothing, Option[QueueItem]] =
    for {
      empty  <- queue.isEmpty
      result <- if (empty) available.update(connection :: _) *> STM.none
                else queue.take.map(Some(_))
    } yield result

  private val tryTake: STM[Nothing, Either[QueueItem, ResettableConnection]] =
    for {
      headOption <- available.get.map(_.headOption)
      either     <- headOption match {
                      case None =>
                        for {
                          promise <- TPromise.make[Nothing, ResettableConnection]
                          ref     <- TRef.make[Boolean](false)
                          item     = QueueItem(promise, ref)
                          _       <- queue.offer(item)
                        } yield Left(item)

                      case Some(connection) =>
                        available.update(_.tail) *> ZSTM.succeed(Right(connection))
                    }
    } yield either
}

private[sql] final class ResettableConnection(val connection: Connection, resetter: Connection => Unit) {
  def reset: UIO[Any] = ZIO.succeed(resetter(connection))
}
