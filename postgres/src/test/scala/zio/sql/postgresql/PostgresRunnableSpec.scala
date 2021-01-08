package zio.sql.postgresql

import zio.sql.Jdbc
import zio.test._
import zio.test.environment.TestEnvironment
import zio.Has

trait PostgresRunnableSpec extends DefaultRunnableSpec with Jdbc with PostgresModule with JdbcExecutorLayer {

  private val layer = TestEnvironment.live >+> executorLayer

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] = specLayered.provideCustomLayerShared(layer)

  def specLayered: Spec[TestEnvironment with Has[TransactionExecutor.Service] with Has[
    ReadExecutor.Service
  ], TestFailure[Object], TestSuccess]

}
