package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.MSSQLServerContainer
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  def container[C <: SingleContainer[_]: Tag](c: C): ZLayer[Any, Throwable, C] =
    ZLayer.scoped {
      ZIO.acquireRelease {
        ZIO.attemptBlocking {
          c.start()
          c
        }
      }(container => ZIO.attemptBlocking(container.stop()).refineToOrDie)
    }

  def postgres(
    imageName: String = "mcr.microsoft.com/mssql/server:2017-latest"
  ): ZIO[Scope, Throwable, MSSQLServerContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new MSSQLServerContainer(
          dockerImageName = DockerImageName.parse(imageName)
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
