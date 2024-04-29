package zio.sql.mysql

import zio.Cause
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.sql.Jdbc
import java.util.UUID
import java.time.LocalDate
import zio.schema._
import zio.sql.expr.FunctionDef.{ CharLength => _, _ }
import zio.sql.table._

object CommonFunctionDefSpec extends MysqlRunnableSpec with Jdbc {

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = Table.defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  private def collectAndCompare[R, E](
    expected: Seq[String],
    testResult: ZStream[R, E, String]
  ) =
    assertZIO(testResult.runCollect)(hasSameElementsDistinct(expected))

  override def specLayered = suite("MySQL Common FunctionDef")(
    suite("Schema dependent tests")(
      test("concat_ws #2 - combine columns") {

        // note: you can't use customerId here as it is a UUID, hence not a string in our book
        val query = select(ConcatWs3(fName, fName, lName)) from customers

        val expected = Seq(
          "RonaldRonaldRussell",
          "TerrenceTerrenceNoel",
          "MilaMilaPaterso",
          "RobertRobertRupert",
          "AlanaAlanaMurray",
          "JoseJoseWiggins"
        )

        val testResult = execute(query)
        collectAndCompare(expected, testResult)
      },
      test("concat_ws #3 - combine columns and flat values") {

        val query = select(ConcatWs4(" ", "Person:", fName, lName)) from customers

        val expected = Seq(
          "Person: Ronald Russell",
          "Person: Terrence Noel",
          "Person: Mila Paterso",
          "Person: Robert Rupert",
          "Person: Alana Murray",
          "Person: Jose Wiggins"
        )

        val testResult = execute(query)
        collectAndCompare(expected, testResult)
      },
      test("concat_ws #3 - combine function calls together") {

        val query = select(
          ConcatWs3(" and ", Concat("Name: ", fName), Concat("Surname: ", lName))
        ) from customers

        val expected = Seq(
          "Name: Ronald and Surname: Russell",
          "Name: Terrence and Surname: Noel",
          "Name: Mila and Surname: Paterso",
          "Name: Robert and Surname: Rupert",
          "Name: Alana and Surname: Murray",
          "Name: Jose and Surname: Wiggins"
        )

        val testResult = execute(query)
        collectAndCompare(expected, testResult)
      },
      test("lower") {
        val query = select(Lower(fName)) from customers limit (1)

        val expected = "ronald"

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("Can concat strings with concat function") {

        val query = select(Concat(fName, lName) as "fullname") from customers

        val expected = Seq("RonaldRussell", "TerrenceNoel", "MilaPaterso", "RobertRupert", "AlanaMurray", "JoseWiggins")

        val result = execute(query)

        val assertion = for {
          r <- result.runCollect
        } yield assert(r)(hasSameElementsDistinct(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("replace") {
        val lastNameReplaced = Replace(lName, "ll", "_") as "lastNameReplaced"
        val computedReplace  = Replace("special ::ąę::", "ąę", "__") as "computedReplace"

        val query = select(lastNameReplaced, computedReplace) from customers

        val expected = ("Russe_", "special ::__::")

        val testResult =
          execute(query).map { case row =>
            (row._1, row._2)
          }

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      }
    ),
    suite("Schema independent tests")(
      test("concat_ws #1 - combine flat values") {

        // note: a plain number (3) would and should not compile
        val query = select(ConcatWs4("+", "1", "2", "3"))

        val expected = Seq("1+2+3")

        val testResult = execute(query)
        collectAndCompare(expected, testResult)
      },
      test("ltrim") {
        assertZIO(execute(select(Ltrim("  hello  "))).runHead.some)(equalTo("hello  "))
      },
      test("rtrim") {
        assertZIO(execute(select(Rtrim("  hello  "))).runHead.some)(equalTo("  hello"))
      },
      test("abs") {
        assertZIO(execute(select(Abs(-3.14159))).runHead.some)(equalTo(3.14159))
      },
      test("log") {
        assertZIO(execute(select(Log(2.0, 32.0))).runHead.some)(equalTo(5.0))
      },
      test("acos") {
        assertZIO(execute(select(Acos(-1.0))).runHead.some)(equalTo(3.141592653589793))
      },
      test("asin") {
        assertZIO(execute(select(Asin(0.5))).runHead.some)(equalTo(0.5235987755982989))
      },
      test("ln") {
        assertZIO(execute(select(Ln(3.0))).runHead.some)(equalTo(1.0986122886681097))
      },
      test("atan") {
        assertZIO(execute(select(Atan(10.0))).runHead.some)(equalTo(1.4711276743037347))
      },
      test("cos") {
        assertZIO(execute(select(Cos(3.141592653589793))).runHead.some)(equalTo(-1.0))
      },
      test("exp") {
        assertZIO(execute(select(Exp(1.0))).runHead.some)(equalTo(2.718281828459045))
      },
      test("floor") {
        assertZIO(execute(select(Floor(-3.14159))).runHead.some)(equalTo(-4.0))
      },
      test("ceil") {
        assertZIO(execute(select(Ceil(53.7), Ceil(-53.7))).runHead.some)(equalTo((54.0, -53.0)))
      },
      test("sin") {
        assertZIO(execute(select(Sin(1.0))).runHead.some)(equalTo(0.8414709848078965))
      },
      test("sqrt") {
        val query = select(Sqrt(121.0))

        val expected = 11.0

        val testResult = execute(query)

        assertZIO(testResult.runHead.some)(equalTo(expected))
      },
      test("round") {
        val query = select(Round(10.8124, 2))

        val expected = 10.81

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign positive") {
        val query = select(Sign(3.0))

        val expected = 1

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign negative") {
        val query = select(Sign(-3.0))

        val expected = -1

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign zero") {
        val query = select(Sign(0.0))

        val expected = 0

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("power") {
        val query = select(Power(7.0, 3.0))

        val expected = 343.000000000000000

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("mod") {
        val query = select(Mod(-15.0, -4.0))

        val expected = -3.0

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("octet_length") {
        val query = select(OctetLength("josé"))

        val expected = 5

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("ascii") {
        val query = select(Ascii("""x"""))

        val expected = 120

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("upper") {
        val query = (select(Upper("ronald"))).limit(1)

        val expected = "RONALD"

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("tan") {
        val query = select(Tan(0.7853981634))

        val expected = 1.0000000000051035

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("trim") {
        assertZIO(execute(select(Trim(" 1234  "))).runHead.some)(equalTo("1234"))
      },
      test("lower") {
        assertZIO(execute(select(Lower("YES"))).runHead.some)(equalTo("yes"))
      }
    )
  )

}
