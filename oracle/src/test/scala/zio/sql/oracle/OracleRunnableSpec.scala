package zio.sql.oracle

import com.dimafeng.testcontainers.{ JdbcDatabaseContainer, OracleContainer, SingleContainer }
import org.testcontainers.utility.DockerImageName
import zio.sql.JdbcRunnableSpec

trait OracleRunnableSpec extends JdbcRunnableSpec with OracleJdbcModule {

  override protected def getContainer: SingleContainer[_] with JdbcDatabaseContainer =
    new OracleContainer(
      dockerImageName = DockerImageName.parse("gvenzl/oracle-xe")
    ).configure { container =>
      container.withInitScript("shop_schema.sql")
      ()
    }

}
