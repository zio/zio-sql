package zio.sql.postgresql

import zio.sql.Jdbc
import zio.test.DefaultRunnableSpec

trait PostgresRunnableSpec extends DefaultRunnableSpec with Jdbc with PostgresModule with JdbcExecutorLayer
