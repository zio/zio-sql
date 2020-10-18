package zio.sql.postgres

import java.util.Properties

import zio.Has
import zio.blocking.Blocking
import zio.sql.{ Jdbc, TestContainer }

trait PostgresIntegrationTestBase { self: Jdbc =>

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val containerName = "postgres:13"

  val postgresLayer = Blocking.live >>> TestContainer.postgres(Some(containerName))
  
  private val poolConfigLayer = postgresLayer.map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))
  
  val connectionPoolLayer = (Blocking.live ++ poolConfigLayer) >>> ConnectionPool.live

}