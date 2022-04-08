package zio.sql.mysql

import com.dimafeng.testcontainers.MySQLContainer
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  def mysql(imageName: String = "mysql"): ZIO[Scope, Throwable, MySQLContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new MySQLContainer(
          mysqlImageVersion = Option(imageName).map(DockerImageName.parse)
        ).configure { a =>
          a.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }
    }(container => ZIO.attemptBlocking(container.stop()).orDie)
}
