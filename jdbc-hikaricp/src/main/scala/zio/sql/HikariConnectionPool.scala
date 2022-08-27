package zio.sql
import com.zaxxer.hikari.{ HikariConfig, HikariDataSource }
import zio.{ Scope, ZIO, ZLayer }

import java.sql.{ Connection, SQLException }

class HikariConnectionPool private (hikariDataSource: HikariDataSource) extends ConnectionPool {

  private[sql] val dataSource = hikariDataSource

  /**
   * Retrieves a JDBC java.sql.Connection as a [[ZIO[Scope, Exception, Connection]]] resource.
   * The managed resource will safely acquire and release the connection, and
   * may be interrupted or timed out if necessary.
   */
  override def connection: ZIO[Scope, Exception, Connection] =
    ZIO.acquireRelease(ZIO.attemptBlocking(hikariDataSource.getConnection).refineToOrDie[SQLException])(con =>
      ZIO.attemptBlocking(hikariDataSource.evictConnection(con)).orDie
    )
}

object HikariConnectionPool {

  private[sql] def initDataSource(config: HikariConfig): ZIO[Scope, Throwable, HikariDataSource] =
    ZIO.acquireRelease(ZIO.attemptBlocking(new HikariDataSource(config)))(ds => ZIO.attemptBlocking(ds.close()).orDie)

  val live: ZLayer[HikariConnectionPoolConfig, Throwable, HikariConnectionPool] =
    ZLayer.scoped {
      for {
        config     <- ZIO.service[HikariConnectionPoolConfig]
        dataSource <- initDataSource(config.toHikariConfig)
        pool        = new HikariConnectionPool(dataSource)
      } yield pool
    }
}
