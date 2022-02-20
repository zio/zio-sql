package zio.sql

import zio.test.environment.TestEnvironment
import zio.test.DefaultRunnableSpec
import zio.ZLayer
import zio.blocking.Blocking
import zio.clock.Clock
import zio.Has

trait JdbcRunnableSpec extends DefaultRunnableSpec with Jdbc {

  type JdbcEnvironment = TestEnvironment with Has[SqlDriver]

  val poolConfigLayer: ZLayer[Blocking, Throwable, Has[ConnectionPoolConfig]]

  final lazy val executorLayer = {
    val connectionPoolLayer: ZLayer[Blocking with Clock, Throwable, Has[ConnectionPool]] =
      ((Blocking.any >+> poolConfigLayer) ++ Clock.any) >>> ConnectionPool.live

    (Blocking.any ++ connectionPoolLayer >+> SqlDriver.live).orDie
  }

  final lazy val jdbcLayer = TestEnvironment.live >+> executorLayer
}
