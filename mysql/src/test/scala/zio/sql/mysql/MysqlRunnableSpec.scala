package zio.sql.mysql

import java.util.Properties

import zio.ZEnvironment
import zio.sql._
import zio.test._

trait MysqlRunnableSpec extends JdbcRunnableSpec with MysqlModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer = TestContainer
    .mysql()
    .map(a =>
      ZEnvironment(
        ConnectionPoolConfig(
          a.get.jdbcUrl,
          connProperties(a.get.username, a.get.password)
        )
      )
    )

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]

}
