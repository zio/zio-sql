package zio.sql.sqlserver

import com.dimafeng.testcontainers.{ MSSQLServerContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.{ Scope, Tag, ZIO, ZLayer }

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

  def sqlServer(
    imageName: String = "mcr.microsoft.com/azure-sql-edge:latest"
  ): ZIO[Scope, Throwable, MSSQLServerContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new MSSQLServerContainer(
          dockerImageName = DockerImageName.parse(imageName).asCompatibleSubstituteFor("mcr.microsoft.com/mssql/server")
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
