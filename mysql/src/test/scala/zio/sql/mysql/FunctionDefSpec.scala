package zio.sql.mysql

import zio.Cause
import zio.test._
import java.util.UUID
import java.time.LocalDate
import zio.schema.DeriveSchema

object FunctionDefSpec extends MysqlRunnableSpec {

  import FunctionDef._
  import MysqlFunctionDef._

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  override def specLayered = suite("MySQL FunctionDef")(
    test("lower") {
      val query = select(Lower(fName)) from customers limit (1)

      val expected = "ronald"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)

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
    //      } yield assertTrue(r.head ==expected)
    //
    //      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    //    },
    test("sin") {
      val query = select(Sin(1.0))

      val expected = 0.8414709848078965

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("abs") {
      val query = select(Abs(-32.0))

      val expected = 32.0

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("crc32") {
      val query = select(Crc32("MySQL")) from customers

      val expected = 3259397556L

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("degrees") {
      val query = select(Degrees(Math.PI)) from customers

      val expected = 180d

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("log2") {
      val query = select(Log2(8d)) from customers

      val expected = 3d

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("log10") {
      val query = select(Log10(1000000d)) from customers

      val expected = 6d

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("bit_length") {
      val query = select(BitLength("hello"))

      val expected = 40

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    },
    test("pi") {
      val query = select(Pi) from customers

      val expected = 3.141593d

      val testResult = execute(query)

      for {
        r <- testResult.runCollect
      } yield assertTrue(r.head == expected)
    }
  )
}
