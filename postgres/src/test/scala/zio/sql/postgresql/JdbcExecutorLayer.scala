package zio.sql.postgresql

import java.util.Properties
import zio.sql.TestContainer
import zio.Has
import zio.ZLayer
import zio.blocking.Blocking
import zio.sql.Jdbc

trait JdbcExecutorLayer { self: Jdbc =>
  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  lazy val executorLayer = {
    val poolConfigLayer = TestContainer
      .postgres("postgres:alpine:13")
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = ZLayer.identity[Blocking] >+> poolConfigLayer >>> ConnectionPool.live

    (ZLayer.identity[
      Blocking
    ] ++ connectionPoolLayer >+> ReadExecutor.live >+> UpdateExecutor.live >+> DeleteExecutor.live >+> TransactionExecutor.live).orDie
  }
}
