package zio.sql.mysql

import zio.sql.Jdbc

trait MysqlJdbcModule extends MysqlRenderModule with Jdbc
