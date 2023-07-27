package zio.sql.oracle

import zio.Cause
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.sql.expr.FunctionDef.{ CharLength => _, _ }

object CommonFunctionDefSpec extends OracleRunnableSpec with ShopSchema with DualSchema {
  import Customers._
  import Dual._

  private def collectAndCompare[R, E](
    expected: Seq[String],
    testResult: ZStream[R, E, String]
  ) =
    assertZIO(testResult.runCollect)(hasSameElementsDistinct(expected))

  override def specLayered = suite("Oracle Common FunctionDef")(
    suite("Schema dependent tests")(
      test("concat - combine function calls together") {

        val query = select(
          Concat(Concat("Name: ", Customers.fName), Concat(" and Surname: ", Customers.lName))
        ) from customers

        val expected = Seq(
          "Name: Ronald and Surname: Russell",
          "Name: Terrence and Surname: Noel",
          "Name: Mila and Surname: Paterso",
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

        val expected = Seq("RonaldRussell", "TerrenceNoel", "MilaPaterso", "AlanaMurray", "JoseWiggins")

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
      test("ltrim") {
        assertZIO(execute(select(Ltrim("  hello  ")).from(dual)).runHead.some)(equalTo("hello  "))
      },
      test("rtrim") {
        assertZIO(execute(select(Rtrim("  hello  ")).from(dual)).runHead.some)(equalTo("  hello"))
      },
      test("abs") {
        assertZIO(execute(select(Abs(-3.14159)).from(dual)).runHead.some)(equalTo(3.14159))
      },
      test("log") {
        assertZIO(execute(select(Log(2.0, 32.0)).from(dual)).runHead.some)(equalTo(5.0))
      },
      test("acos") {
        assertZIO(execute(select(Acos(-1.0)).from(dual)).runHead.some)(equalTo(3.141592653589793))
      },
      test("asin") {
        assertZIO(execute(select(Asin(0.5)).from(dual)).runHead.some)(equalTo(0.5235987755982989))
      },
      test("ln") {
        assertZIO(execute(select(Ln(3.0)).from(dual)).runHead.some)(equalTo(1.0986122886681097))
      },
      test("atan") {
        assertZIO(execute(select(Atan(10.0)).from(dual)).runHead.some)(equalTo(1.4711276743037347))
      },
      test("cos") {
        assertZIO(execute(select(Cos(3.141592653589793)).from(dual)).runHead.some)(equalTo(-1.0))
      },
      test("exp") {
        assertZIO(execute(select(Exp(1.0)).from(dual)).runHead.some)(equalTo(2.718281828459045))
      },
      test("floor") {
        assertZIO(execute(select(Floor(-3.14159)).from(dual)).runHead.some)(equalTo(-4.0))
      },
      test("ceil") {
        assertZIO(execute(select(Ceil(53.7), Ceil(-53.7)).from(dual)).runHead.some)(equalTo((54.0, -53.0)))
      },
      test("sin") {
        assertZIO(execute(select(Sin(1.0)).from(dual)).runHead.some)(equalTo(0.8414709848078965))
      },
      test("sqrt") {
        val query    = select(Sqrt(121.0)).from(dual)
        val expected = 11.0

        val testResult = execute(query)

        assertZIO(testResult.runHead.some)(equalTo(expected))
      },
      test("round") {
        val query = select(Round(10.8124, 2)).from(dual)

        val expected = 10.81

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign positive") {
        val query = select(Sign(3.0)).from(dual)

        val expected = 1

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign negative") {
        val query = select(Sign(-3.0)).from(dual)

        val expected = -1

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("sign zero") {
        val query = select(Sign(0.0)).from(dual)

        val expected = 0

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("power") {
        val query = select(Power(7.0, 3.0)).from(dual)

        val expected = 343.000000000000000

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("mod") {
        val query = select(Mod(-15.0, -4.0)).from(dual)

        val expected = -3.0

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("octet_length") {
        val query = select(OctetLength("josé")).from(dual)

        val expected = 5

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      } @@ TestAspect.ignore @@ TestAspect.tag("lengthb"),
      test("ascii") {
        val query = select(Ascii("""x""")).from(dual)

        val expected = 120

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("upper") {
        val query = (select(Upper("ronald")).from(dual)).limit(1)

        val expected = "RONALD"

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("width_bucket") {
        val query = select(WidthBucket(5.35, 0.024, 10.06, 5)).from(dual)

        val expected = 3

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("tan") {
        val query = select(Tan(0.7853981634)).from(dual)

        val expected = 1.0000000000051035

        val testResult = execute(query)

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("trim") {
        assertZIO(execute(select(Trim(" 1234  ")).from(dual)).runHead.some)(equalTo("1234"))
      },
      test("lower") {
        assertZIO(execute(select(Lower("YES")).from(dual)).runHead.some)(equalTo("yes"))
      }
    )
  )

}
