package zio.sql

import zio.test.TestAspect.{ sequential, timeout, withLiveClock }
import zio.test.{ TestEnvironment, _ }
import zio.{ durationInt, ZIO, ZLayer }

object HikariConnectionPoolSpec extends ZIOSpecDefault {

  val mySqlConfigLayer: ZLayer[Any, Throwable, MySqlConfig] =
    ZLayer.scoped {
      MySqlTestContainer
        .mysql()
        .map(a =>
          MySqlConfig(
            url = a.jdbcUrl,
            username = a.username,
            password = a.password
          )
        )
    }

  val hikariPoolConfigLayer: ZLayer[MySqlConfig, Nothing, HikariConnectionPoolConfig] =
    ZLayer.fromFunction((conf: MySqlConfig) =>
      HikariConnectionPoolConfig(url = conf.url, userName = conf.username, password = conf.password)
    )
  val poolLayer: ZLayer[HikariConnectionPoolConfig, Nothing, HikariConnectionPool]    = HikariConnectionPool.live.orDie

  override def spec: Spec[TestEnvironment, Any] =
    specLayered.provideCustomShared(mySqlConfigLayer.orDie)

  def specLayered: Spec[TestEnvironment with MySqlConfig, Any] =
    suite("Hikaricp module")(
      test("Pool size should be configurable") {
        val poolSize = 20
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getMaximumPoolSize == poolSize))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(poolSize = poolSize))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("Pool size should have 10 connections by default") {
        (for {
          cp <- ZIO.service[HikariConnectionPool]
          _  <- ZIO.replicateZIO(10)(ZIO.scoped(cp.connection))
        } yield assertTrue(cp.dataSource.getMaximumPoolSize == 10))
          .provideSomeLayer[TestEnvironment with MySqlConfig](hikariPoolConfigLayer >>> poolLayer)
      } @@ timeout(10.minutes) @@ withLiveClock,
      test("It should be possible to acquire connections from the pool") {
        val poolSize = 20
        (for {
          cp <- ZIO.service[HikariConnectionPool]
          _  <-
            ZIO.collectAllParDiscard(ZIO.replicate(poolSize)(ZIO.scoped(cp.connection *> ZIO.sleep(500.millisecond))))
        } yield assert("")(Assertion.anything)).provideSomeLayer[TestEnvironment with MySqlConfig](
          hikariPoolConfigLayer.map(_.update(_.copy(poolSize = poolSize))) >>> poolLayer
        )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("Auto commit should be configurable") {
        val autoCommit = false
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.isAutoCommit == autoCommit))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(autoCommit = autoCommit))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("Auto commit should be true by default") {
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.isAutoCommit))
          .provideSomeLayer[TestEnvironment with MySqlConfig](hikariPoolConfigLayer >>> poolLayer)
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("Connection timeout should be configurable") {
        val connectionTimeout = 2000L
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getConnectionTimeout == connectionTimeout))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(connectionTimeout = Some(connectionTimeout)))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("Idle timeout should be configurable") {
        val idleTimeout = 2000L
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getIdleTimeout == idleTimeout))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(idleTimeout = Some(idleTimeout)))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("initialization fail timeout should be configurable") {
        val initializationFailTimeout = 2000L
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getInitializationFailTimeout == initializationFailTimeout))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(
              _.update(_.copy(initializationFailTimeout = Some(initializationFailTimeout)))
            ) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("max lifetime should be configurable") {
        val maxLifetime = 40000L
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getMaxLifetime == maxLifetime))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(maxLifetime = Some(maxLifetime)))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("minimum idle should be configurable") {
        val minimumIdle = 2
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getMinimumIdle == minimumIdle))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(minimumIdle = Some(minimumIdle)))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock,
      test("connection init SQL should be configurable") {
        val initialSql = "SELECT 1 FROM test.test"
        (for {
          cp <- ZIO.service[HikariConnectionPool]
        } yield assertTrue(cp.dataSource.getConnectionInitSql == initialSql))
          .provideSomeLayer[TestEnvironment with MySqlConfig](
            hikariPoolConfigLayer.map(_.update(_.copy(connectionInitSql = Some(initialSql)))) >>> poolLayer
          )
      } @@ timeout(10.seconds) @@ withLiveClock
    ) @@ sequential
}
