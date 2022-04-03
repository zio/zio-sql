package zio.sql

import zio._
import zio.test.TestAspect.{ sequential, timeout }
import zio.test.{ TestEnvironment, _ }

object ConnectionPoolSpec extends ZIOSpecDefault with TestContainer {

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomShared((poolConfigLayer >>> ConnectionPool.live).orDie)

  def specLayered: Spec[TestEnvironment with ConnectionPool, TestFailure[Object], TestSuccess] =
    suite("Postgres module")(
      test("Fibers waiting for connections can be interrupted") {
        // We need to actually sleep here to make sure that the started fibers had started acquiring connections.
        ZIO.scoped {
          for {
            cp      <- ZIO.service[ConnectionPool]
            promise <- Promise.make[Nothing, Unit]
            _       <- ZIO.replicateZIO(poolSize)(cp.connection.flatMap(_ => promise.await).fork)
            _       <- ZIO.sleep(1.second)
            waiting <- ZIO.replicateZIO(poolSize)(cp.connection.fork)
            _       <- ZIO.sleep(1.second)
            _       <- ZIO.foreach(waiting)(_.interrupt)
            _       <- promise.complete(ZIO.unit)
            _       <- cp.connection
          } yield assert("")(Assertion.anything)
        }
      } @@ timeout(10.seconds)
    ) @@ sequential
}
