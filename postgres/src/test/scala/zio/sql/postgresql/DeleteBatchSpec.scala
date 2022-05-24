package zio.sql.postgresql

import zio.Cause
import zio.test.Assertion._
import zio.test._

object DeleteBatchSpec extends PostgresRunnableSpec with DbSchema {

  import Customers._

  override def specLayered = suite("Postgres module Batchdelete")(
    test("Can delete more than one customer from single table with a condition") {
      val query = deleteFrom(customers).where(verified.isNotTrue)

      val result = executeBatch(List(query))

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(List(1)))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can delete 4 customer from single table with a condition") {
      val query = deleteFrom(customers).where(verified.isTrue)

      val result = executeBatch(List(query)).map(l=>l.reduce( (a,b) => a+b))

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(4))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
    )

}
