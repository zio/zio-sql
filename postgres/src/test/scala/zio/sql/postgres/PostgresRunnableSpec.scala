package zio.sql.postgres

import java.util.Properties

import zio.{Has, ZEnv, ZLayer}
import zio.blocking.Blocking
import zio.sql.JdbcRunnableSpec
import zio.sql.TestContainer
import zio.test.environment.TestEnvironment

trait PostgresRunnableSpec extends JdbcRunnableSpec with PostgresModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  private val executorLayer = {
    val poolConfigLayer = TestContainer
      .postgres("postgres:alpine:13")
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = ZLayer.identity[Blocking] >+> poolConfigLayer >>> ConnectionPool.live

    (ZLayer.identity[
      Blocking
    ] ++ connectionPoolLayer >+> ReadExecutor.live >+> UpdateExecutor.live >+> DeleteExecutor.live >+> TransactionExecutor.live).orDie
  }

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, Environment] =
    TestEnvironment.live >+> executorLayer

}
