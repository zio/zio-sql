package zio.sql.postgresql

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

object Postgres {
  type PostgresTestEnvironment =
    Annotations
      with Live
      with Sized
      with TestClock
      with TestConfig
      with TestConsole
      with TestRandom
      with TestSystem
      with ZEnv

  object PostgresTestEnvironment {
    val any: ZLayer[PostgresTestEnvironment, Nothing, PostgresTestEnvironment] =
      ZLayer.requires[PostgresTestEnvironment]
    lazy val live: ZLayer[ZEnv, Nothing, PostgresTestEnvironment] =
      Annotations.live ++
        Blocking.live ++
        Live.default ++
        Sized.live(100) ++
        ((Live.default ++ Annotations.live) >>> TestClock.default) ++
        TestConfig.live(100, 100, 200, 1000) ++
        (Live.default >>> TestConsole.debug) ++
        TestRandom.deterministic ++
        TestSystem.default
  }
}

abstract class PostgresRunnableSpec extends RunnableSpec[Postgres.PostgresTestEnvironment, Any] {

  override def aspects: List[TestAspect[Nothing, Postgres.PostgresTestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[Postgres.PostgresTestEnvironment, Any] =
    defaultTestRunner

  /**
   * Returns an effect that executes a given spec, producing the results of the execution.
   */
  private[zio] override def runSpec(
    spec: ZSpec[Environment, Failure]
  ): URIO[TestLogger with Clock, ExecutedSpec[Failure]] =
    runner.run(aspects.foldLeft(spec)(_ @@ _) @@ TestAspect.fibers)
}
