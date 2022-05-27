package zio.sql.oracle

import com.dimafeng.testcontainers.OracleContainer
import org.testcontainers.utility.DockerImageName
import zio.{Scope, ZIO}

object TestContainer {

  def oracle(imageName: String = "gvenzl/oracle-xe"): ZIO[Scope, Throwable, OracleContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = new OracleContainer(
          dockerImageName = DockerImageName.parse(imageName)
        ).configure { container =>
          container.withInitScript("shop_schema.sql")
          ()
        }
        c.start()
        c
      }
    }(container => ZIO.attemptBlocking(container.stop()).orDie)
}
