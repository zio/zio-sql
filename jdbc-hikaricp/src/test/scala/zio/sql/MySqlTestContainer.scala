package zio.sql

import com.dimafeng.testcontainers.MySQLContainer
import org.testcontainers.utility.DockerImageName
import zio._

final case class MySqlConfig(username: String, password: String, url: String)
object MySqlTestContainer {

  def mysql(imageName: String = "mysql"): ZIO[Scope, Throwable, MySQLContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new MySQLContainer(
          mysqlImageVersion = Option(imageName).map(DockerImageName.parse)
        ).configure { a =>
          a.withInitScript("test_schema.sql")
          ()
        }
        c.start()
        c
      }
    }(container => ZIO.attemptBlocking(container.stop()).orDie)
}
