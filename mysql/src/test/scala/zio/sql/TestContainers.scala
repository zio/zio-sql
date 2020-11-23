package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.MySQLContainer
import zio._
import zio.blocking.{ effectBlocking, Blocking }

// TODO: copy/pasted from postgres module.  put in common location
object TestContainer {

  def container[C <: SingleContainer[_]: Tag](c: C): ZLayer[Blocking, Throwable, Has[C]] =
    ZManaged.make {
      effectBlocking {
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

  def mysql(imageName: String): ZLayer[Blocking, Throwable, Has[MySQLContainer]] =
    ZManaged.make {
      effectBlocking {
        val c = new MySQLContainer(mysqlImageVersion = Some(imageName)).configure { a =>
          a.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

}
