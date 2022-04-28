package zio.sql.sqlserver

import zio.sql.Jdbc

trait SqlServerJdbcModule extends SqlServerRenderModule with Jdbc
