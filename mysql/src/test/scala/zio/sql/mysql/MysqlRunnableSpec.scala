package zio.sql.mysql

import java.util.Properties

import zio.{ Has, ZEnv, ZLayer }
import zio.blocking.Blocking
import zio.sql.JdbcRunnableSpec
import zio.sql.TestContainer
import zio.test.environment.TestEnvironment

trait MysqlRunnableSpec extends JdbcRunnableSpec with MysqlModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  private val executorLayer = {
    val poolConfigLayer = TestContainer
      .mysql()
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = Blocking.live >+> poolConfigLayer >>> ConnectionPool.live

    (ZLayer.identity[
      Blocking
    ] ++ connectionPoolLayer >+> ReadExecutor.live >+> UpdateExecutor.live >+> DeleteExecutor.live >+> TransactionExecutor.live).orDie
  }

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, Environment] =
    TestEnvironment.live >+> executorLayer

}
