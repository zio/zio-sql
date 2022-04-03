package zio.sql

import com.dimafeng.testcontainers.MySQLContainer
import org.testcontainers.utility.DockerImageName
import zio._
import java.util.Properties

trait MySqlTestContainer extends JdbcRunnableSpec {

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
          val c = new MySQLContainer(
            mysqlImageVersion = Option({ "mysql" }).map(DockerImageName.parse)
          ).configure { a =>
            a.withInitScript("shop_schema.sql")
            ()
          }
          c.start()
          c
        }
      }(container => ZIO.attemptBlocking(container.stop()).orDie)
        .map(c => ConnectionPoolConfig(c.jdbcUrl, connProperties(c.username, c.password)))
    }
}
