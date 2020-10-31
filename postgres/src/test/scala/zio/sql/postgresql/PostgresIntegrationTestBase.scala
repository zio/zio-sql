package zio.sql.postgresql

import java.util.Properties

//import com.dimafeng.testcontainers.PostgreSQLContainer
import zio.Has
import zio.blocking.Blocking
import zio.sql.{ Jdbc, TestContainer }

import zio.URIO
import zio.clock.Clock
import zio.duration._

import zio.blocking.Blocking
import zio.test._
import zio.test.environment._
import zio.{ ZEnv, ZLayer }
import zio.test.Annotations
import zio.test.Sized
import zio.test.TestConfig


object JdbcIntegrationTestBase extends Jdbc {

    type JdbcTestEnvironment =
    Annotations
      with Live
      with Sized
      with TestClock
      with TestConfig
      with TestConsole
      with TestRandom
      with TestSystem
      with ZEnv
      with ReadExecutor
}

trait JdbcIntegrationTestBase { self: Jdbc =>

  type JdbcTestEnvironment =
    Annotations
      with Live
      with Sized
      with TestClock
      with TestConfig
      with TestConsole
      with TestRandom
      with TestSystem
      with ZEnv
      with ReadExecutor

  protected def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  protected def imageName: String

}

trait PostgresIntegrationTestBase extends RunnableSpec[JdbcIntegrationTestBase.JdbcTestEnvironment, Any] with JdbcIntegrationTestBase { self: Jdbc =>

  override def aspects: List[TestAspect[Nothing, JdbcIntegrationTestBase.JdbcTestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[JdbcIntegrationTestBase.JdbcTestEnvironment, Any] = ???
    //defaultTestRunner

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment, Failure]
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _) @@ TestAspect.fibers)

  override val imageName = "postgres:alpine:13"

  private val poolConfigLayer = TestContainer
    .postgres(imageName)
    .map(a => Has(ConnectionPool.Config(a.get.jdbcUrl, connProperties(a.get.username, a.get.password))))

  private val connectionPoolLayer = (Blocking.live ++ poolConfigLayer) >>> ConnectionPool.live

  val executorLayer = Blocking.live >>> ((Blocking.live ++ connectionPoolLayer) >>> ReadExecutor.live)

  object JdbcTestEnvironment {
    val any: ZLayer[JdbcTestEnvironment, Nothing, JdbcTestEnvironment] =
      ZLayer.requires[JdbcTestEnvironment]
    lazy val live: ZLayer[ZEnv, Throwable, JdbcTestEnvironment] =
      Annotations.live ++
        Blocking.live ++
        Live.default ++
        Sized.live(100) ++
        ((Live.default ++ Annotations.live) >>> TestClock.default) ++
        TestConfig.live(100, 100, 200, 1000) ++
        (Live.default >>> TestConsole.debug) ++
        TestRandom.deterministic ++
        TestSystem.default ++
        executorLayer
  }
}
