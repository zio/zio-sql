package zio.sql.oracle

import zio.ZLayer
import zio.sql.{ConnectionPoolConfig, JdbcRunnableSpec}
import zio.test.{Spec, TestEnvironment}

import java.util.Properties

trait OracleRunnableSpec extends JdbcRunnableSpec with OracleJdbcModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer = ZLayer.scoped {
    TestContainer
      .oracle()
      .map(container =>
        ConnectionPoolConfig(
          url = container.jdbcUrl,
          properties = connProperties(container.username, container.password),
        )
      )
  }

  override def spec: Spec[TestEnvironment, Any] =
    specLayered.provideCustomShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, Object]

}
