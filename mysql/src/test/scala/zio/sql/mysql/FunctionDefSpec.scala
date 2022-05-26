package zio.sql.mysql

import zio.test._
import zio.test.Assertion._

import java.time.{ LocalDate, LocalTime, ZoneId }
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

      assertZIO(testResult.runHead.some)(equalTo(expected))
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

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("abs") {
      val query = select(Abs(-32.0))

      val expected = 32.0

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("crc32") {
      val query = select(Crc32("MySQL")) from customers

      val expected = 3259397556L

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("degrees") {
      val query = select(Degrees(Math.PI)) from customers

      val expected = 180d

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("hex") {
      val query       = select(Hex(255L)) from customers
      val expected    = "FF"
      val queryResult = execute(query)

      assertZIO(queryResult.runHead.some)(equalTo(expected))
    },
    test("log2") {
      val query = select(Log2(8d)) from customers

      val expected = 3d

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("log10") {
      val query = select(Log10(1000000d)) from customers

      val expected = 6d

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("now") {
      val timestampFormatter =
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss").withZone(ZoneId.of("UTC"))

      val query = select(Now())

      val testResult = execute(query)

      assertZIO(
        testResult.runHead.some
          .map(t => timestampFormatter.format(t))
      )(matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"))
    },
    test("bit_length") {
      val query = select(BitLength("hello"))

      val expected = 40

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("current_date") {
      val query = select(CurrentDate)

      val expected = LocalDate.now()

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("maketime") {
      val query = select(MakeTime(12, 15, 30.5)) from customers

      val expected = LocalTime.parse("12:15:30.5")

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("pi") {
      val query = select(Pi) from customers

      val expected = 3.141593d

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("rand") {
      val query = select(Rand(5))

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(isGreaterThanEqualTo(0d) && isLessThanEqualTo(1d))
    },
    test("rpad") {
      val cases = Seq(("hi", 5, "?", "hi???"), ("hi", 1, "?", "h"))
      check(Gen.fromIterable(cases)) { case (str, len, pad, exp) =>
        assertZIO(execute(select(RPad(str, len, pad))).runHead.some)(equalTo(exp))
      }
    },
    test("current_time") {
      assertZIO(
        execute(select(CurrentTime)).runHead.some
          .map(t => DateTimeFormatter.ofPattern("HH:mm:ss").format(t))
      )(matchesRegex("(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]"))
    }
  )
}
