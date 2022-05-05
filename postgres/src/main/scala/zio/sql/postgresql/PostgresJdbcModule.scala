package zio.sql.postgresql

import zio.sql.Jdbc

trait PostgresJdbcModule extends PostgresRenderModule with Jdbc
