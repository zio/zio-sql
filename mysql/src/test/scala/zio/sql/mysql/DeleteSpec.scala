package zio.sql.mysql

import zio.Cause
import zio.test.Assertion._
import zio.test._

object DeleteSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered = suite("MySQL module delete")(
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers).where(verified.isNotTrue)

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(1))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
