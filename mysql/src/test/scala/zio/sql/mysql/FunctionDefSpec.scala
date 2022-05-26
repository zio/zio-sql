package zio.sql.mysql

import zio.Cause
import zio.test._
import zio.test.Assertion._
import java.time.LocalDate

import java.time.{ LocalTime, ZoneId }
import java.time.format.DateTimeFormatter

object FunctionDefSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._
  import FunctionDef._
  import MysqlFunctionDef._

  override def specLayered = suite("MySQL FunctionDef")(
    test("lower") {
      val query = select(Lower(fName)) from customers limit (1)

      val expected = "ronald"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    // FIXME: lower with string literal should not refer to a column name
    // See: https://www.w3schools.com/sql/trymysql.asp?filename=trysql_func_mysql_lower
    // Uncomment the following test when fixed
    //    test("lower with string literal") {
    //      val query = select(Lower("LOWER")) from customers limit(1)
    //
    //      val expected = "lower"
    //
    //      val testResult = execute(query.to[String, String](identity))
    //
    //      val assertion = for {
    //        r <- testResult.runCollect
    //      } yield assert(r.head)(equalTo(expected))
    //
    //      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    //    },
    test("sin") {
      val query = select(Sin(1.0))

      val expected = 0.8414709848078965

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("abs") {
      val query = select(Abs(-32.0))

      val expected = 32.0

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("crc32") {
      val query = select(Crc32("MySQL")) from customers

      val expected = 3259397556L

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("degrees") {
      val query = select(Degrees(Math.PI)) from customers

      val expected = 180d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("log2") {
      val query = select(Log2(8d)) from customers

      val expected = 3d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("log10") {
      val query = select(Log10(1000000d)) from customers

      val expected = 6d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("now") {
      val timestampFormatter =
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss").withZone(ZoneId.of("UTC"))

      val query = select(Now())

      val testResult = execute(query)

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("bit_length") {
      val query = select(BitLength("hello"))

      val expected = 40

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("current_date") {
      val query = select(CurrentDate)

      val expected = LocalDate.now()

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("maketime") {
      val query = select(MakeTime(12, 15, 30.5)) from customers

      val expected = LocalTime.parse("12:15:30.5")

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("pi") {
      val query = select(Pi) from customers

      val expected = 3.141593d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
