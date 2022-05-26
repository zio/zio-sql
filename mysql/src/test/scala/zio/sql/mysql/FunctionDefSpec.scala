package zio.sql.mysql

import zio.Cause
import zio.test._
import zio.test.Assertion._

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
    test("soundex outputs should not match for non-similar-sounding strings") {
      val queryForRobert = select(Soundex("Robert"))
      val queryForTam    = select(Soundex("Tam"))

      val resultForRobert = execute(queryForRobert)
      val resultForTam    = execute(queryForTam)

      val assertion = for {
        robertResult <- resultForRobert.runCollect
        tamResult    <- resultForTam.runCollect
      } yield assert(robertResult.head.equals(tamResult.head))(equalTo(false))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("soundex outputs should match for equivalent strings") {
      val queryForRobert = select(Soundex("Robert"))
      val queryForRupert = select(Soundex("Rupert"))

      val resultForRobert = execute(queryForRobert)
      val resultForRupert = execute(queryForRupert)

      val assertion = for {
        robertResult <- resultForRobert.runCollect
        rupertResult <- resultForRupert.runCollect
      } yield assert(robertResult.head.equals(rupertResult.head))(equalTo(true))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("soundex") {
      val query    = select(Soundex("Robert"))
      val expected = "R163"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

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
