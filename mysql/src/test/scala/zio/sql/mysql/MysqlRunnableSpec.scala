package zio.sql.mysql

import java.util.Properties

import zio.sql._
import zio.test._
import zio.ZLayer

trait MysqlRunnableSpec extends JdbcRunnableSpec with MysqlModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig] = TestContainer
    .mysql()
    .map(a => ConnectionPoolConfig(a.jdbcUrl, connProperties(a.username, a.password)))
    .toLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomLayerShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]

}
