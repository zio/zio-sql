package zio.sql.postgresql

import java.time.{ Instant, LocalTime }

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
    testM("localtime") {
      val query = select(Localtime()) from customers

      val testResult = execute(query).to[LocalTime, LocalTime](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head.toString)(Assertion.matchesRegex("([0-9]{2}):[0-9]{2}:[0-9]{2}\\.[0-9]{3}"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtime with precision") {
      val precision = 0
      val query     = select(LocaltimeWithPrecision(precision)) from customers

      val testResult = execute(query).to[LocalTime, LocalTime](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head.toString)(Assertion.matchesRegex(s"([0-9]{2}):[0-9]{2}:[0-9].[0-9]{$precision}"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtimestamp") {
      val query = select(Localtimestamp()) from customers

      val testResult = execute(query).to[Instant, Instant](identity)

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(r.head.toString)(
          Assertion.matchesRegex("([0-9]{4})-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6}Z")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtimestamp with precision") {
      val precision = 2

      val millis =
        if (precision == 0) ""
        else if (precision <= 3) List.fill(3)("[0-9]").mkString(".", "", "")
        else List.fill(6)("[0-9]").mkString(".", "", "")

      val query = select(LocaltimestampWithPrecision(precision)) from customers

      val testResult = execute(query).to[Instant, Instant](identity)

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(r.head.toString)(
          Assertion.matchesRegex(s"([0-9]{4})-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}${millis}Z")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
