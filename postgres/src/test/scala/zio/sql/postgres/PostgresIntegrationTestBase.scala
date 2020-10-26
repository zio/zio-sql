package zio.sql.postgres

import java.util.Properties

import com.dimafeng.testcontainers.PostgreSQLContainer
import zio.Has
import zio.blocking.Blocking
import zio.sql.{ Jdbc, TestContainer }

trait JdbcIntegrationTestBase { self: Jdbc =>

  protected def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  def imageName: String

}

trait PostgresIntegrationTestBase extends JdbcIntegrationTestBase { self: Jdbc =>

  override val imageName = "postgres:alpine:13"

  val container = 
    new PostgreSQLContainer(
      dockerImageNameOverride = Some(imageName),
    )
    .configure { a => 
      a.withInitScript("shop_schema.sql")
      ()
    }

  val dbLayer = Blocking.live >>> TestContainer.container(container)

  private val poolConfigLayer = dbLayer.map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))
  
  val connectionPoolLayer = (Blocking.live ++ poolConfigLayer) >>> ConnectionPool.live

  val executorLayer = ((Blocking.live ++ connectionPoolLayer) >>> ReadExecutor.live)
}