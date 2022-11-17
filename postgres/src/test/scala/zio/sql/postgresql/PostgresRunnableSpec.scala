package zio.sql.postgresql

import com.dimafeng.testcontainers.{ JdbcDatabaseContainer, PostgreSQLContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.sql.JdbcRunnableSpec

trait PostgresRunnableSpec extends JdbcRunnableSpec with PostgresJdbcModule {

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

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]

}
