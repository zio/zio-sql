package zio.sql.sqlserver

import com.dimafeng.testcontainers.MSSQLServerContainer 
import org.testcontainers.utility.DockerImageName
import zio._

object TestContainer {

  /**
    * We are using Azure sql edge because MS Sql Server image won't run on ARM.
    */
  val sqlServer: ZIO[Scope, Throwable, MSSQLServerContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new MSSQLServerContainer(
          dockerImageName = DockerImageName.parse("mcr.microsoft.com/azure-sql-edge:latest").asCompatibleSubstituteFor("mcr.microsoft.com/mssql/server")
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
