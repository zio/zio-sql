package zio.sql.sqlserver

import zio.test._
import java.util.Properties
import zio.sql.{ ConnectionPoolConfig, JdbcRunnableSpec, TestContainer }

trait SqlServerRunnableSpec extends JdbcRunnableSpec with SqlServerModule {

  def autoCommit: Boolean = true

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer = TestContainer
    .postgres()
    .map(a =>
      ConnectionPoolConfig(
        url = a.jdbcUrl,
        properties = connProperties(a.username, a.password),
        autoCommit = autoCommit
      )
    )
    .toLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomLayerShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]
}
