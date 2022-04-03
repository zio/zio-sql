package zio.sql.postgresql

import zio.test._
import zio.sql.PostgresqlTestContainer

trait PostgresRunnableSpec extends PostgresqlTestContainer with PostgresModule {

  override def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    specLayered.provideCustomShared(jdbcLayer)

  def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess]

}
