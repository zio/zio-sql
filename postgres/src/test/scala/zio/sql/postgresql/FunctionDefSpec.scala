package zio.sql.postgresql

import zio.Cause
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect.ignore

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
    test("custom expr rendering") {
      val query  = select(Overlay("w333333rce", "resou", 3, 5)) from customers
      val render = renderRead(query) //SELECT overlay(w333333rce placing resou from 3 for 5) FROM customers
      assert(render)(equalTo("SELECT overlay('w333333rce' placing 'resou' from 3 for 5) FROM customers"))
    } @@ ignore, //todo fix rendering of strings
    testM("sin") {
      val query = select(Sin(1.0)) from customers

      val expected = 0.8414709848078965

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sind") {
      val query = select(Sind(30.0)) from customers

      val expected = 0.5

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
