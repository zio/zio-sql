package zio.sql

import com.dimafeng.testcontainers.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  def postgres(imageName: String = "postgres:alpine"): ZIO[Scope, Throwable, PostgreSQLContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new PostgreSQLContainer(
          dockerImageNameOverride = Option(imageName).map(DockerImageName.parse)
        )
        c.start()
        c
      }
    } { container =>
      ZIO.attemptBlocking(container.stop()).orDie
    }

}
