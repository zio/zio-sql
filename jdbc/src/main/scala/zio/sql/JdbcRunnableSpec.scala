package zio.sql

import zio.test.environment.TestEnvironment
import zio.test.DefaultRunnableSpec
import zio.ZLayer
import zio.blocking.Blocking
import zio.Has

trait JdbcRunnableSpec extends DefaultRunnableSpec with Jdbc {

  type JdbcEnvironment = TestEnvironment
    with ReadExecutor
    with UpdateExecutor
    with DeleteExecutor
    with TransactionExecutor

  val poolConfigLayer: ZLayer[Blocking, Throwable, Has[ConnectionPool.Config]]

  final lazy val executorLayer = {
    val connectionPoolLayer = ZLayer.identity[Blocking] >+> poolConfigLayer >>> ConnectionPool.live

    (ZLayer.identity[
      Blocking
    ] ++ connectionPoolLayer >+> ReadExecutor.live >+> UpdateExecutor.live >+> DeleteExecutor.live >+> TransactionExecutor.live).orDie
  }

  final lazy val jdbcLayer = TestEnvironment.live >+> executorLayer
}
