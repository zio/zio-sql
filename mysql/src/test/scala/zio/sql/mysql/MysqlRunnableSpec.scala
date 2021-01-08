package zio.sql.mysql

import java.util.Properties

import zio._
import zio.blocking.Blocking
import zio.sql._
import zio.test.environment.TestEnvironment
import zio.test._

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

  private val layer = TestEnvironment.live >+> executorLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] = specLayered.provideCustomLayerShared(layer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]

}
