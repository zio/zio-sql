package zio.sql.postgresql

import zio._
import zio.test._
import java.util.Properties
import zio.sql.{ ConnectionPoolConfig, JdbcRunnableSpec }

trait PostgresRunnableSpec extends JdbcRunnableSpec with PostgresJdbcModule {

  def autoCommit: Boolean = true

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer = ZLayer.scoped {
    TestContainer
      .postgres()
      .map(a =>
        ConnectionPoolConfig(
          url = a.jdbcUrl,
          properties = connProperties(a.username, a.password),
          autoCommit = autoCommit
        )
      )
  }

  override def spec: Spec[TestEnvironment, Any] =
    specLayered.provideCustomShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, Object]

}
