package zio.sql.mysql

import com.dimafeng.testcontainers.{ JdbcDatabaseContainer, MySQLContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.sql.JdbcRunnableSpec

trait MysqlRunnableSpec extends JdbcRunnableSpec with MysqlJdbcModule {

  override protected def getContainer: SingleContainer[_] with JdbcDatabaseContainer =
    new MySQLContainer(
      mysqlImageVersion = Option("mysql:8.2.0").map(DockerImageName.parse)
    ).configure { a =>
      a.withInitScript("shop_schema.sql")
      ()
    }

}
