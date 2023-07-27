package zio.sql.sqlserver

import com.dimafeng.testcontainers.{ JdbcDatabaseContainer, MSSQLServerContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.sql.JdbcRunnableSpec

trait SqlServerRunnableSpec extends JdbcRunnableSpec with SqlServerJdbcModule {

  override protected def getContainer: SingleContainer[_] with JdbcDatabaseContainer =
    new MSSQLServerContainer(
      dockerImageName = DockerImageName
        .parse("mcr.microsoft.com/azure-sql-edge:latest")
        .asCompatibleSubstituteFor("mcr.microsoft.com/mssql/server")
    ).configure { a =>
      a.withInitScript("db_schema.sql")
      ()
    }

}
