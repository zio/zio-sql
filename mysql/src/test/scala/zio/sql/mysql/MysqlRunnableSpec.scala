package zio.sql.mysql

import java.util.Properties

import zio.blocking.Blocking
import zio.sql.TestContainer
import zio.sql.postgresql.JdbcRunnableSpec
import zio.test.environment.TestEnvironment
import zio.{ Has, ZEnv, ZLayer }

trait MysqlRunnableSpec extends JdbcRunnableSpec with MysqlModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  private val executorLayer = {
    val poolConfigLayer = TestContainer
      .mysql("mysql:8")
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = Blocking.live >+> poolConfigLayer >>> ConnectionPool.live

    (Blocking.live ++ connectionPoolLayer >>> ReadExecutor.live).orDie
  }

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, TestEnvironment with ReadExecutor] =
    TestEnvironment.live ++ executorLayer

}
