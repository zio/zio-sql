package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
import zio._
import zio.blocking.{ effectBlocking, Blocking }

object TestContainer {

  def container[C <: SingleContainer[_]: Tag](c: C): ZLayer[Blocking, Nothing, Has[C]] =
    ZManaged.make {
      effectBlocking {
        c.start()
        c
      }.orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer

  def postgres(imageName: String): ZLayer[Blocking, Nothing, Has[PostgreSQLContainer]] =
    ZManaged.make {
      effectBlocking {
        val c = new PostgreSQLContainer(
          dockerImageNameOverride = Some(imageName),
        ).configure { a => 
          a.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }.orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer


}
