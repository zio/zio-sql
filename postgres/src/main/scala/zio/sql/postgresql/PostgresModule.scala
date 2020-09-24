package zio.sql.postgresql

import zio.sql.{ Jdbc, Sql }

trait PostgresModule extends Sql with Jdbc {}
