package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
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

  def postgres(
    imageName: Option[String] = Some("postgres:alpine:13")
  ): ZLayer[Blocking, Throwable, Has[PostgreSQLContainer]] =
    ZManaged.make {
      effectBlocking {
        val c = new PostgreSQLContainer(
          dockerImageNameOverride = imageName
        ).configure { a =>
          a.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }
    } { container =>
      effectBlocking(container.stop()).orDie
    }.toLayer

}
