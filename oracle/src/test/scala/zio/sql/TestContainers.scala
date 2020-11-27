package zio.sql

import com.dimafeng.testcontainers.SingleContainer
import com.dimafeng.testcontainers.OracleContainer
import zio._
import zio.blocking.{ effectBlocking, Blocking }
object TestContainer {

  def container[C <: SingleContainer[_]: Tag](c: C): ZLayer[Blocking, Throwable, Has[C]] =
    ZManaged.make {
      effectBlocking {
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

  /* NOTE: TestContainer is not building remotely: Caused by: java.sql.SQLException: ORA-00604: error occurred at recursive SQL level 1
  [info] ORA-01882: timezone region not found.
  This happens only on Oracle DB container testing.
  */
  
  def oracle(imageName: String): ZLayer[Blocking, Throwable, Has[OracleContainer]] =
    ZManaged.make {
      effectBlocking {
        val c = new OracleContainer(
          dockerImageName = imageName
        ).configure { a =>
          a.withInitScript("shop_schema.sql")
          a.addEnv("TZ", "America/New_York")
          ()
        }
        c.start()
        c
      }
    }(container => effectBlocking(container.stop()).orDie).toLayer

}
