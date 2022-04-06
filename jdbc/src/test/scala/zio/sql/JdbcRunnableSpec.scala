package zio.sql

import zio.test.TestEnvironment
import zio.ZLayer
import zio.test.ZIOSpecDefault
import zio.test.TestFailure

trait JdbcRunnableSpec extends ZIOSpecDefault with Jdbc {

  type JdbcEnvironment = TestEnvironment with SqlDriver

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig]

  final lazy val jdbcLayer: ZLayer[Any, TestFailure[Any], SqlDriver] =
    ZLayer.make[SqlDriver](
      poolConfigLayer.orDie,
      ConnectionPool.live.orDie,
      SqlDriver.live
    )
}
