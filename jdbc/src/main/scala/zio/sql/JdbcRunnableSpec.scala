package zio.sql

import zio.{ ZEnv, ZLayer }
import zio.duration._
import zio.test._
import zio.test.environment.TestEnvironment

trait JdbcRunnableSpec extends AbstractRunnableSpec with Jdbc {

  override type Environment = TestEnvironment with ReadExecutor
  override type Failure     = Any

  override def aspects: List[TestAspect[Nothing, TestEnvironment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  def jdbcTestEnvironment: ZLayer[ZEnv, Nothing, TestEnvironment with ReadExecutor]

  override def runner: TestRunner[TestEnvironment with ReadExecutor, Any] =
    TestRunner(TestExecutor.default(ZEnv.live >>> jdbcTestEnvironment))

}
