package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  def container[C <: SingleContainer[_]: Tag: IsNotIntersection](c: C): ZLayer[Any, Throwable, C] =
    ZManaged.acquireReleaseWith {
      ZIO.attemptBlocking {
        c.start()
        c
      }
    }(container => ZIO.attemptBlocking(container.stop()).orDie).toLayer

  def postgres(imageName: String = "postgres:alpine"): ZManaged[Any, Throwable, PostgreSQLContainer] =
    ZManaged.acquireReleaseWith {
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
