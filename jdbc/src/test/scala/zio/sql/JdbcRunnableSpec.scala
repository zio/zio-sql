package zio.sql

import zio.test.TestEnvironment
import zio.test.DefaultRunnableSpec
import zio.ZLayer
import zio.Clock

trait JdbcRunnableSpec extends DefaultRunnableSpec with Jdbc {

  type JdbcEnvironment = TestEnvironment with SqlDriver

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig]

  final lazy val executorLayer = {
    val connectionPoolLayer: ZLayer[Clock, Throwable, ConnectionPool] =
      (poolConfigLayer ++ Clock.any) >>> ConnectionPool.live

    (connectionPoolLayer >+> SqlDriver.live).orDie
  }

  final lazy val jdbcLayer = TestEnvironment.live >>> executorLayer
}
