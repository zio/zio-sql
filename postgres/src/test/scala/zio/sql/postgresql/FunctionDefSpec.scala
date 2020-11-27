package zio.sql.postgresql

import java.time.OffsetTime

import zio.Cause
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
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
    },
    testM("timeofday") {
      val query = select(Timeofday) from customers

      val testResult = execute(query).to[String, String](identity)

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(r.head)(
          matchesRegex(
            "[A-Za-z]{3}\\s[A-Za-z]{3}\\s[0-9]{2}\\s(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9].[0-9]{6}\\s[0-9]{4}\\s[A-Za-z]{3}"
          )
        )
      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("current_time") {
      val query = select(CurrentTime()) from customers

      val testResult = execute(query).to[OffsetTime, OffsetTime](identity)

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(r.head.toString)(
          matchesRegex(
            "(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]Z"
          )
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
