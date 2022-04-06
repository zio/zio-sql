package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.MySQLContainer
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  def container[C <: SingleContainer[_]: Tag: IsNotIntersection](c: C): ZLayer[Any, Throwable, C] =
    ZLayer.scoped {
      ZIO.acquireRelease {
        ZIO.attemptBlocking {
          c.start()
          c
        }
      }(container => ZIO.attemptBlocking(container.stop()).refineToOrDie)
    }

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
