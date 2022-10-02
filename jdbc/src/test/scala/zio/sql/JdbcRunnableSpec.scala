package zio.sql

import com.dimafeng.testcontainers.JdbcDatabaseContainer
import zio.test.TestEnvironment
import zio.{ Scope, ZIO, ZLayer }
import zio.test.ZIOSpecDefault
import zio.prelude.AssociativeBoth
import zio.test.Gen
import zio.prelude.Covariant
import com.dimafeng.testcontainers.SingleContainer
import java.util.Properties
import zio.test.Spec

/**
  * Base trait for integration-style tests running on Testcontainers.
  * Extending classes are expected to provide the container implementation 
  * this test suite will work on by implementing {@link getContainer}.
  * 
  * Test suite should be implemented in {@link specLayered} and 
  * particular tests can depend on {@link SQLDriver} in the environment.
  */
trait JdbcRunnableSpec extends ZIOSpecDefault with Jdbc {

  type JdbcEnvironment = TestEnvironment with SqlDriver

  def specLayered: Spec[JdbcEnvironment, Object]

  protected def getContainer: SingleContainer[_] with JdbcDatabaseContainer

  protected val autoCommit = false

  override def spec: Spec[TestEnvironment, Any] =
    specLayered.provideCustomShared(jdbcLayer)

  private[this] def connProperties(user: String, password: String): Properties = {
    val props = new Properties
    props.setProperty("user", user)
    props.setProperty("password", password)
    props
  }

  private[this] val poolConfigLayer: ZLayer[Any, Throwable, ConnectionPoolConfig] =
    ZLayer.scoped {
      testContainer
        .map(a =>
          ConnectionPoolConfig(
            url = a.jdbcUrl,
            properties = connProperties(a.username, a.password),
            autoCommit = autoCommit
          )
        )
    }

  val connectionPool: ZLayer[Any, Throwable, ConnectionPool] = poolConfigLayer >>> ConnectionPool.live

  private[this] final lazy val jdbcLayer: ZLayer[Any, Any, SqlDriver] =
    ZLayer.make[SqlDriver](
      connectionPool.orDie,
      SqlDriver.live
    )

  protected implicit def genInstances[R]
    : AssociativeBoth[({ type T[A] = Gen[R, A] })#T] with Covariant[({ type T[+A] = Gen[R, A] })#T] =
    new AssociativeBoth[({ type T[A] = Gen[R, A] })#T] with Covariant[({ type T[+A] = Gen[R, A] })#T] {
      def map[A, B](f: A => B): Gen[R, A] => Gen[R, B]                   = _.map(f)
      def both[A, B](fa: => Gen[R, A], fb: => Gen[R, B]): Gen[R, (A, B)] = fa.zip(fb)
    }

  val testContainer: ZIO[Scope, Throwable, SingleContainer[_] with JdbcDatabaseContainer] =
    ZIO.acquireRelease {
      ZIO.attemptBlocking {
        val c = getContainer
        c.start()
        c
      }
    } { container =>
      ZIO.attemptBlocking(container.stop()).orDie
    }
}
