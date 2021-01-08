package zio.sql.postgresql

import zio.test._
import zio.test.environment.TestEnvironment
import java.util.Properties
import zio.sql.TestContainer
import zio.Has
import zio.ZLayer
import zio.blocking.Blocking
import zio.sql.Jdbc

trait PostgresRunnableSpec extends DefaultRunnableSpec with Jdbc with PostgresModule {

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  lazy val executorLayer = {
    val poolConfigLayer = TestContainer
      .postgres()
      .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

    val connectionPoolLayer = ZLayer.identity[Blocking] >+> poolConfigLayer >>> ConnectionPool.live

    (ZLayer.identity[
      Blocking
    ] ++ connectionPoolLayer >+> ReadExecutor.live >+> UpdateExecutor.live >+> DeleteExecutor.live >+> TransactionExecutor.live).orDie
  }

  private val layer = TestEnvironment.live >+> executorLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] = specLayered.provideCustomLayerShared(layer)

  //TODO use object Environment from jdbcrunnable
  def specLayered: Spec[TestEnvironment with TransactionExecutor with ReadExecutor, TestFailure[Object], TestSuccess]

}
