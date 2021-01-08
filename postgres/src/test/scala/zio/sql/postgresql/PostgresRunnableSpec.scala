package zio.sql.postgresql

import zio.sql.Jdbc
import zio.test._
import zio.test.environment.TestEnvironment

trait PostgresRunnableSpec extends DefaultRunnableSpec with Jdbc with PostgresModule with JdbcExecutorLayer {

  private val layer = TestEnvironment.live >+> executorLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] = specLayered.provideCustomLayerShared(layer)

  def specLayered: Spec[TestEnvironment with TransactionExecutor with ReadExecutor, TestFailure[Object], TestSuccess]

}
