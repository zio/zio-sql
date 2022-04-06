package zio.sql

import zio.{ durationInt, Promise, ZIO, ZLayer }
import zio.test.TestAspect.{ sequential, timeout, withLiveClock }
import zio.test.{ TestEnvironment, _ }

import java.util.Properties

object ConnectionPoolSpec extends ZIOSpecDefault {

  val poolSize = 10

  private def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig] =
    ZLayer.scoped {
      TestContainer
        .postgres()
        .map(a =>
          ConnectionPoolConfig(
            url = a.jdbcUrl,
            properties = connProperties(a.username, a.password),
            poolSize = poolSize
          )
        )
    }

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomShared((poolConfigLayer >>> ConnectionPool.live).orDie)

  def specLayered: Spec[TestEnvironment with ConnectionPool, TestFailure[Object], TestSuccess] =
    suite("Postgres module")(
      test("Fibers waiting for connections can be interrupted") {
        // We need to actually sleep here to make sure that the started fibers
        // had started acquiring connections.
        for {
          cp      <- ZIO.service[ConnectionPool]
          promise <- Promise.make[Nothing, Unit]
          _       <- ZIO.replicateZIO(poolSize)(ZIO.scoped(cp.connection *> promise.await).fork)
          _       <- ZIO.sleep(1.second)
          waiting <- ZIO.replicateZIO(poolSize)(ZIO.scoped(cp.connection).fork)
          _       <- ZIO.sleep(1.second)
          _       <- ZIO.foreach(waiting)(_.interrupt)
          _       <- promise.complete(ZIO.unit)
          _       <- ZIO.scoped(cp.connection)
        } yield assert("")(Assertion.anything)
      } @@ timeout(10.seconds) @@ withLiveClock
    ) @@ sequential
}
