package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.MySQLContainer
import org.testcontainers.utility.DockerImageName
import zio._
import zio.blocking.{ effectBlocking, Blocking }

object TestContainer {

  def container[C <: SingleContainer[_]: Tag](c: C): ZLayer[Blocking, Throwable, Has[C]] =
    ZManaged.make {
      effectBlocking {
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

  def mysql(imageName: String = "mysql"): ZLayer[Blocking, Throwable, Has[MySQLContainer]] =
    ZManaged.make {
      effectBlocking {
        val c = new MySQLContainer(
          mysqlImageVersion = Option(imageName).map(DockerImageName.parse)
        ).configure { a =>
          a.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

}
