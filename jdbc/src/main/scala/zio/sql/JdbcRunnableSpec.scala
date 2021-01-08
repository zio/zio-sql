package zio.sql

import zio.test.environment.TestEnvironment
import zio.test.DefaultRunnableSpec

trait JdbcRunnableSpec extends DefaultRunnableSpec with Jdbc {

  type JdbcEnvironment = TestEnvironment
    with ReadExecutor
    with UpdateExecutor
    with DeleteExecutor
    with TransactionExecutor

}
