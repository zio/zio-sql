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

  private val executorLayer: ZLayer[Blocking, Nothing, ReadExecutor with DeleteExecutor] = {
    val poolConfigLayer = TestContainer
      .mysql("mysql:8")
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = ZLayer.identity[Blocking] >+> poolConfigLayer >>> ConnectionPool.live
    (ZLayer.identity[Blocking] ++ connectionPoolLayer >+> ReadExecutor.live >+> DeleteExecutor.live).orDie
  }

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, TestEnvironment with ReadExecutor with DeleteExecutor] =
    TestEnvironment.live ++ executorLayer

}
