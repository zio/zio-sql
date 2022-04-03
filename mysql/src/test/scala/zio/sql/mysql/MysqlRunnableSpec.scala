package zio.sql.mysql

import zio.sql._
import zio.test._

trait MysqlRunnableSpec extends MySqlTestContainer with MysqlModule {

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomLayerShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]
}