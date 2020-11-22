package zio.sql.oracle

import zio.{ Has, ZEnv, ZLayer }
import zio.blocking.Blocking
import zio.sql.TestContainer
import zio.test.environment.TestEnvironment

import java.util.Properties
import zio.sql.JdbcRunnableSpec

trait OracleRunnableSpec extends JdbcRunnableSpec with OracleModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  private val executorLayer = {
    val poolConfigLayer = TestContainer
      .oracle("oracleinanutshell/oracle-xe-11g")
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = Blocking.live >+> poolConfigLayer >>> ConnectionPool.live

    (Blocking.live ++ connectionPoolLayer >>> ReadExecutor.live).orDie
  }

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, TestEnvironment with ReadExecutor] =
    TestEnvironment.live ++ executorLayer

}
