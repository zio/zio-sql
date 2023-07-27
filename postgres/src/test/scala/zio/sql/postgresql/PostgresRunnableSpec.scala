package zio.sql.postgresql

import com.dimafeng.testcontainers.{ JdbcDatabaseContainer, PostgreSQLContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.sql.JdbcRunnableSpec

trait PostgresRunnableSpec extends JdbcRunnableSpec with PostgresJdbcModule {

  override protected def getContainer: SingleContainer[_] with JdbcDatabaseContainer =
    new PostgreSQLContainer(
      dockerImageNameOverride = Option("postgres:alpine").map(DockerImageName.parse)
    ).configure { a =>
      a.withInitScript("db_schema.sql")
      ()
    }

}
