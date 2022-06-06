package zio.sql

import zio.test.TestEnvironment
import zio.ZLayer
import zio.test.ZIOSpecDefault
import zio.prelude.AssociativeBoth
import zio.test.Gen
import zio.prelude.Covariant

trait JdbcRunnableSpec extends ZIOSpecDefault with Jdbc {

  type JdbcEnvironment = TestEnvironment with SqlDriver

  val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig]

  final lazy val jdbcLayer: ZLayer[Any, Any, SqlDriver] =
    ZLayer.make[SqlDriver](
      poolConfigLayer.orDie,
      ConnectionPool.live.orDie,
      SqlDriver.live
    )

  protected implicit def genInstances[R]
    : AssociativeBoth[({ type T[A] = Gen[R, A] })#T] with Covariant[({ type T[+A] = Gen[R, A] })#T] =
    new AssociativeBoth[({ type T[A] = Gen[R, A] })#T] with Covariant[({ type T[+A] = Gen[R, A] })#T] {
      def map[A, B](f: A => B): Gen[R, A] => Gen[R, B]                   = _.map(f)
      def both[A, B](fa: => Gen[R, A], fb: => Gen[R, B]): Gen[R, (A, B)] = fa.zip(fb)
    }
}
