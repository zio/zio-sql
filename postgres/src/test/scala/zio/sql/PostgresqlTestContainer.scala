package zio.sql

import com.dimafeng.testcontainers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName
import zio._
import java.util.Properties

trait PostgresqlTestContainer extends JdbcRunnableSpec {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig] =
    ZLayer.scoped {
      ZIO.acquireRelease {
        ZIO.attemptBlocking {
          val c = new PostgreSQLContainer(
            dockerImageNameOverride = Option("postgres:alpine").map(DockerImageName.parse)
          ).configure { a =>
            a.withInitScript("db_schema.sql")
            ()
          }
          c.start()
          c
        }
      } { container =>
        ZIO.attemptBlocking(container.stop()).orDie
      }.map(c => ConnectionPoolConfig(c.jdbcUrl, connProperties(c.username, c.password)))

    }


}
