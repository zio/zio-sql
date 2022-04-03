package zio.sql.sqlserver

import zio.test._
import zio.sql.SqlServerTestContainer

trait SqlServerRunnableSpec extends SqlServerTestContainer with SqlServerModule {
  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomLayerShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]
}
