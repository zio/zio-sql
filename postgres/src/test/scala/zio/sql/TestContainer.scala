package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
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
      }(container => ZIO.attemptBlocking(container.stop()).orDie)
    }

  def postgres(imageName: String = "postgres:alpine"): ZIO[Scope, Throwable, PostgreSQLContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new PostgreSQLContainer(
          dockerImageNameOverride = Option(imageName).map(DockerImageName.parse)
        ).configure { a =>
          a.withInitScript("db_schema.sql")
          ()
        }
        c.start()
        c
      }
    } { container =>
      ZIO.attemptBlocking(container.stop()).orDie
    }

}
