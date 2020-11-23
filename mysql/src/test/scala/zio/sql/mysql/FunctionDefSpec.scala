package zio.sql.mysql

import zio.Cause
import zio.test.Assertion._
import zio.test._

object FunctionDefSpec extends MysqlRunnableSpec with ShopSchema {
  import this.Customers._
  import this.FunctionDef._

  val spec = suite("Mysql FunctionDef")(
    testM("lower") {
      val query = select(Lower("first_name")) from customers limit (1)

      val expected = "ronald"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sin") {
      val query = select(Sin(1.0)) from customers

      val expected = 0.8414709848078965

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
