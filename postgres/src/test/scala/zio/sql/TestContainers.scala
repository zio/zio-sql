package zio.sql

import com.dimafeng.testcontainers.PostgreSQLContainer
import zio.blocking.{effectBlocking, Blocking}
import zio._

object TestContainer {
  type Postgres = Has[PostgreSQLContainer]
  
  def postgres(imageName: Option[String] = Some("postgres")): ZLayer[Blocking, Nothing, Postgres] =
    ZManaged.make {
      effectBlocking {
        val container = new PostgreSQLContainer(
          dockerImageNameOverride = imageName,
        )
        .configure { a => 
          a.withInitScript("shop_schema.sql")
          ()
        }
        container.start()
        container
      }.orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer
}
