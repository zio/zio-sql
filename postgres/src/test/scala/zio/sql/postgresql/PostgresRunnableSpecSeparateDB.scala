package zio.sql.postgresql

import zio.{ ZEnv, ZLayer }
import zio.test.environment.TestEnvironment
import zio.sql.JdbcRunnableSpec

trait PostgresRunnableSpecSeparateDB extends JdbcRunnableSpec with PostgresModule with JdbcExecutorLayer {

  override val jdbcTestEnvironment: ZLayer[ZEnv, Nothing, Environment] =
    TestEnvironment.live >+> executorLayer

}
