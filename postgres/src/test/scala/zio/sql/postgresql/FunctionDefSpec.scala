package zio.sql.postgresql

import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect.{ ignore, timeout }
import zio.test._
import zio.{ durationInt, Cause, Chunk }

import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID

object FunctionDefSpec extends PostgresRunnableSpec with DbSchema {

  import Customers._
  import FunctionDef.{ CharLength => _, _ }
  import PostgresFunctionDef._
  import PostgresSpecific._

  private def collectAndCompare[R, E](
    expected: Seq[String],
    testResult: ZStream[R, E, String]
  ) =
    assertZIO(testResult.runCollect)(equalTo(expected))

  private val timestampFormatter = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSSS").withZone(ZoneId.of("UTC"))

  override def specLayered = suite("Postgres FunctionDef")(
    test("concat_ws #1 - combine flat values") {
      import Expr._

      // note: a plain number (3) would and should not compile
      val query = select(ConcatWs4("+", "1", "2", "3"))

      val expected = Seq("1+2+3")

      val testResult = execute(query)
      collectAndCompare(expected, testResult)
    },
    test("concat_ws #2 - combine columns") {

      // note: you can't use customerId here as it is a UUID, hence not a string in our book
      val query = select(ConcatWs3(Customers.fName, Customers.fName, Customers.lName)) from customers

      val expected = Seq(
        "RonaldRonaldRussell",
        "TerrenceTerrenceNoel",
        "MilaMilaPaterso",
        "AlanaAlanaMurray",
        "JoseJoseWiggins"
      )

      val testResult = execute(query)
      collectAndCompare(expected, testResult)
    },
    test("concat_ws #3 - combine columns and flat values") {
      import Expr._

      val query = select(ConcatWs4(" ", "Person:", Customers.fName, Customers.lName)) from customers

      val expected = Seq(
        "Person: Ronald Russell",
        "Person: Terrence Noel",
        "Person: Mila Paterso",
        "Person: Alana Murray",
        "Person: Jose Wiggins"
      )

      val testResult = execute(query)
      collectAndCompare(expected, testResult)
    },
    test("concat_ws #3 - combine function calls together") {
      import Expr._

      val query = select(
        ConcatWs3(" and ", Concat("Name: ", Customers.fName), Concat("Surname: ", Customers.lName))
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
    test("isfinite") {
      val query             = select(IsFinite(Instant.now))
      val expected: Boolean = true
      assertZIO(execute(query).runHead.some)(equalTo(expected))
    },
    suite("String functions")(
      test("CharLength") {
        assertZIO(execute(select(Length("hello"))).runHead.some)(equalTo(5))
      },
      test("CharLength") {
        assertZIO(execute(select(Length("hello"))).runHead.some)(equalTo(5))
      },
      test("ltrim") {
        assertZIO(execute(select(Ltrim("  hello  "))).runHead.some)(equalTo("hello  "))
      },
      test("rtrim") {
        assertZIO(execute(select(Rtrim("  hello  "))).runHead.some)(equalTo("  hello"))
      },
      test("bit_length") {
        assertZIO(execute(select(BitLength("hello"))).runHead.some)(equalTo(40))
      },
      test("pi") {
        assertZIO(execute(select(Pi)).runHead.some)(equalTo(3.141592653589793))
      },
      test("gen_random_uuid") {
        assertZIO(execute(select(GenRandomUuid)).runHead.some)(!isNull)
      },
      suite("format function")(
        test("format0") {
          import Expr._

          val query = select(Format0("Person"))

          val expected = Seq("Person")

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        },
        test("format1") {
          import Expr._

          val query = select(Format1("Person: %s", Customers.fName)) from customers

          val expected = Seq(
            "Person: Ronald",
            "Person: Terrence",
            "Person: Mila",
            "Person: Alana",
            "Person: Jose"
          )

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        },
        test("format2") {
          import Expr._

          val query = select(Format2("Person: %s %s", Customers.fName, Customers.lName)) from customers

          val expected = Seq(
            "Person: Ronald Russell",
            "Person: Terrence Noel",
            "Person: Mila Paterso",
            "Person: Alana Murray",
            "Person: Jose Wiggins"
          )

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        },
        test("format3") {
          import Expr._

          val query = select(
            Format3("Person: %s %s with double quoted %I ", Customers.fName, Customers.lName, "identi fier")
          ) from customers

          val expected = Seq(
            s"""Person: Ronald Russell with double quoted "identi fier" """,
            s"""Person: Terrence Noel with double quoted "identi fier" """,
            s"""Person: Mila Paterso with double quoted "identi fier" """,
            s"""Person: Alana Murray with double quoted "identi fier" """,
            s"""Person: Jose Wiggins with double quoted "identi fier" """
          )

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        },
        test("format4") {
          import Expr._

          val query = select(
            Format4(
              "Person: %s %s with null-literal %L and non-null-literal %L ",
              Customers.fName,
              Customers.lName,
              "FIXME: NULL",
              "literal"
            )
          ) from customers

          val expected = Seq(
            s"""Person: Ronald Russell with null-literal 'FIXME: NULL' and non-null-literal 'literal' """,
            s"""Person: Terrence Noel with null-literal 'FIXME: NULL' and non-null-literal 'literal' """,
            s"""Person: Mila Paterso with null-literal 'FIXME: NULL' and non-null-literal 'literal' """,
            s"""Person: Alana Murray with null-literal 'FIXME: NULL' and non-null-literal 'literal' """,
            s"""Person: Jose Wiggins with null-literal 'FIXME: NULL' and non-null-literal 'literal' """
          )

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        },
        test("format5") {
          import Expr._

          val query = select(
            Format5(
              "Person: %s %s with more arguments than placeholders: %I %L ",
              Customers.fName,
              Customers.lName,
              "identifier",
              Reverse(Customers.fName),
              "unused"
            )
          ) from customers

          val expected = Seq(
            s"""Person: Ronald Russell with more arguments than placeholders: identifier 'dlanoR' """,
            s"""Person: Terrence Noel with more arguments than placeholders: identifier 'ecnerreT' """,
            s"""Person: Mila Paterso with more arguments than placeholders: identifier 'aliM' """,
            s"""Person: Alana Murray with more arguments than placeholders: identifier 'analA' """,
            s"""Person: Jose Wiggins with more arguments than placeholders: identifier 'esoJ' """
          )

          val testResult = execute(query)
          collectAndCompare(expected, testResult)
        }
      )
    ),
    test("abs") {
      assertZIO(execute(select(Abs(-3.14159))).runHead.some)(equalTo(3.14159))
    },
    test("log") {
      assertZIO(execute(select(Log(2.0, 32.0))).runHead.some)(equalTo(5.0))
    },
    test("acos") {
      assertZIO(execute(select(Acos(-1.0))).runHead.some)(equalTo(3.141592653589793))
    },
    test("repeat") {
      assertZIO(execute(select(Repeat("Zio", 3))).runHead.some)(equalTo("ZioZioZio"))
    },
    test("reverse") {
      assertZIO(execute(select(Reverse("abcd"))).runHead.some)(equalTo("dcba"))
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
    test("sind") {
      assertZIO(execute(select(Sind(30.0))).runHead.some)(equalTo(0.5))
    },
    test("split_part") {
      assertZIO(execute(select(SplitPart("abc~@~def~@~ghi", "~@~", 2))).runHead.some)(equalTo("def"))
    },
    test("timeofday") {
      assertZIO(execute(select(TimeOfDay())).runHead.some)(
        matchesRegex(
          "[A-Za-z]{3}\\s[A-Za-z]{3}\\s[0-9]{2}\\s(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9].[0-9]{6}\\s[0-9]{4}\\s[A-Za-z]{3,4}"
        )
      )
    },
    test("localtime") {
      assertZIO(execute(select(Localtime)).runHead.some.map(_.toString))(
        matchesRegex(
          "([0-9]{2}):[0-9]{2}:[0-9]{2}\\.[0-9]{6}"
        )
      )
    },
    test("localtime with precision") {
      val precision = 3
      assertZIO(execute(select(LocaltimeWithPrecision(precision))).runHead.some.map(_.toString))(
        matchesRegex(
          s"\\d{2}:\\d{2}:\\d{2}\\.\\d{$precision}"
        )
      )
    },
    test("localtimestamp") {
      assertZIO(execute(select(Localtimestamp)).runHead.some.map(timestampFormatter.format _))(
        matchesRegex(
          "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}"
        )
      )
    },
    test("localtimestamp with precision") {
      assertZIO(execute(select(LocaltimestampWithPrecision(2))).runHead.some.map(timestampFormatter.format _))(
        matchesRegex(
          "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}00"
        )
      )
    },
    test("now") {
      assertZIO(execute(select(Now())).runHead.some.map(timestampFormatter.format _))(
        matchesRegex(
          "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}"
        )
      )
    },
    test("statement_timestamp") {
      assertZIO(execute(select(StatementTimestamp())).runHead.some.map(timestampFormatter.format _))(
        matchesRegex(
          "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}"
        )
      )
    },
    test("transaction_timestamp") {
      assertZIO(execute(select(TransactionTimestamp())).runHead.some.map(timestampFormatter.format _))(
        matchesRegex(
          "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}"
        )
      )
    },
    test("current_time") {
      assertZIO(
        execute(select(TransactionTimestamp())).runHead.some.map(DateTimeFormatter.ofPattern("HH:mm:ss").format(_))
      )(
        matchesRegex(
          "(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]"
        )
      )
    },
    test("md5") {
      val query = select(Md5("hello, world!"))

      val expected = "3adbbad1791fbae3ec908894c4963870"

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    suite("parseIdent")(
      test("parseIdent removes quoting of individual identifiers") {
        val someString: Gen[Sized, String]    = Gen.string
          .filter(x => x.length < 50 && x.length > 1)
        // NOTE: I don't know if property based testing is worth doing here, I just wanted to try it
        val genTestString: Gen[Sized, String] =
          for {
            string1 <- someString
            string2 <- someString
          } yield s""""${string1}".${string2}"""

        check(genTestString) { (testString) =>
          val query      = select(ParseIdent(testString))
          val testResult = execute(query)

          assertZIO(testResult.runHead.some)(not(containsString("'")) && not(containsString("\"")))
        }
      },
      test("parseIdent fails with invalid identifier") {
        val query      = select(ParseIdent("\'\"SomeSchema\".someTable.\'"))
        val testResult = execute(query)

        assertZIO(testResult.runCollect.exit)(fails(anything))
      }
    ) @@ ignore,
    test("sqrt") {
      val query = select(Sqrt(121.0))

      val expected = 11.0

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("chr") {
      val query = select(Chr(65))

      val expected = "A"

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
    test("initcap") {
      val query = select(Initcap("hi THOMAS"))

      val expected = "Hi Thomas"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("trim_scale") {
      val query = select(TrimScale(8.4100))

      val expected = 8.41

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("hex") {
      val query = select(Hex(2147483647))

      val expected = "7fffffff"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("encode") {
      val query = select(Encode(Chunk.fromArray("Hello, World!".getBytes), "BASE64"))

      val expected = "SGVsbG8sIFdvcmxkIQ=="

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("decode") {
      val query = select(Decode("SGVsbG8sIFdvcmxkIQ==", "BASE64"))

      val expected = Chunk.fromArray("Hello, World!".getBytes)

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("trunc") {
      val query = select(Trunc(42.8))

      val expected = 42d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
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
    test("length") {
      val query = select(Length("hello"))

      val expected = 5

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
    test("translate") {
      val query = select(Translate("12345", "143", "ax"))

      val expected = "a2x5"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("left") {
      val query = select(Left("abcde", 2))

      val expected = "ab"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("right") {
      val query = select(Right("abcde", 2))

      val expected = "de"

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("radians") {
      val query = select(Radians(45.0))

      val expected = 0.7853981634

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(approximatelyEquals(expected, 10.0))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("min_scale") {
      val query = select(MinScale(8.4100))

      val expected = 2

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("starts_with") {
      case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query =
        (select(customerId, fName, lName, verified, dob) from customers).where(StartsWith(fName, "R"))

      val expected =
        Seq(
          Customer(
            UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
            "Ronald",
            "Russell",
            true,
            LocalDate.parse("1983-01-05")
          )
        )

      val testResult = execute(query).map { case (id, fname, lname, verified, dob) =>
        Customer(id, fname, lname, verified, dob)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
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
    test("lower with string literal") {
      val query = select(Lower("LOWER")) from customers limit (1)

      val expected = "lower"

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
    test("width_bucket") {
      val query = select(WidthBucket(5.35, 0.024, 10.06, 5))

      val expected = 3

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
    test("gcd") {
      val query = select(GCD(1071d, 462d))

      val expected = 21d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("lcm") {
      val query = select(LCM(1071d, 462d))

      val expected = 23562d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("cbrt") {
      val query = select(CBRT(64.0))

      val expected = 4d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("degrees") {
      val query = select(Degrees(0.5))

      val expected = 28.64788975654116

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("div") {
      val query = select(Div(8d, 4d))

      val expected = 2d

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("factorial") {
      val query = select(Factorial(5))

      val expected = 120

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("random") {
      val query = select(Random())

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(Assertion.isGreaterThanEqualTo(0d) && Assertion.isLessThanEqualTo(1d))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("setseed") {
      val query = select(SetSeed(0.12), Random(), Random())

      val randomTupleForSeed = (0.019967750719779076, 0.8378369929936333)
      val testResult         = execute(query).map { case (_, b, c) => (b, c) }

      assertZIO(testResult.runHead.some)(equalTo(randomTupleForSeed))
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
    test("Can calculate character length of a string") {

      val query = select(CharLength(fName)) from customers

      val expected = Seq(6, 8, 4, 5, 4)

      val result = execute(query)

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElements(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("to_timestamp") {
      val query      = select(ToTimestamp(1284352323L))
      val expected   = ZonedDateTime.of(2010, 9, 13, 4, 32, 3, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val testResult = execute(query)

      val expectedRoundTripTimestamp = ZonedDateTime.of(2020, 11, 21, 19, 10, 25, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val roundTripQuery             =
        select(createdString, createdTimestamp) from customers
      val roundTripResults           = execute(roundTripQuery).map { case row =>
        (row._1, ZonedDateTime.parse(row._1), row._2)
      }
      val roundTripExpected          = List(
        ("2020-11-21T19:10:25+00:00", ZonedDateTime.parse("2020-11-21T19:10:25+00:00"), expectedRoundTripTimestamp),
        ("2020-11-21T15:10:25-04:00", ZonedDateTime.parse("2020-11-21T15:10:25-04:00"), expectedRoundTripTimestamp),
        ("2020-11-22T02:10:25+07:00", ZonedDateTime.parse("2020-11-22T02:10:25+07:00"), expectedRoundTripTimestamp),
        ("2020-11-21T12:10:25-07:00", ZonedDateTime.parse("2020-11-21T12:10:25-07:00"), expectedRoundTripTimestamp)
      )

      val assertion = for {
        single    <- testResult.runCollect
        roundTrip <- roundTripResults.runCollect
      } yield assert(single.head)(equalTo(expected)) &&
        assert(roundTrip)(hasSameElementsDistinct(roundTripExpected))

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
    },
    test("lpad") {
      def runTest(s: String, pad: String) = {
        val query = select(LPad(s, 5, pad))

        for {
          r <- execute(query).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertZIO(runTest("hi", "xy"))(equalTo("xyxhi"))
        t2 <- assertZIO(runTest("hello", "xy"))(equalTo("hello"))
        t3 <- assertZIO(runTest("hello world", "xy"))(equalTo("hello"))
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("rpad") {
      def runTest(s: String, pad: String) = {
        val query = select(RPad(s, 5, pad))

        for {
          r <- execute(query).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertZIO(runTest("hi", "xy"))(equalTo("hixyx"))
        t2 <- assertZIO(runTest("hello", "xy"))(equalTo("hello"))
        t3 <- assertZIO(runTest("hello world", "xy"))(equalTo("hello"))
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("pg_client_encoding") {
      val query = select(PgClientEncoding())

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo("UTF8"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
      @@ ignore, // todo fix - select(PgClientEncoding())?
    test("make_date") {
      val query = select(MakeDate(2013, 7, 15))

      val expected = LocalDate.of(2013, 7, 15)

      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("make_interval") {
      def runTest(interval: Interval) = {
        val query = select(
          MakeInterval(interval)
        )
        for {
          r <- execute(query).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertZIO(runTest(Interval()))(equalTo(Interval()))
        t2 <- assertZIO(runTest(Interval(days = 10)))(equalTo(Interval(days = 10)))
        t3 <- assertZIO(
                runTest(Interval(years = 10, months = 2, days = 5, hours = 6, minutes = 20, seconds = 15))
              )(
                equalTo(Interval(years = 10, months = 2, days = 5, hours = 6, minutes = 20, seconds = 15))
              )
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("make_time") {
      val query      = select(MakeTime(8, 15, 23.5))
      val expected   = LocalTime.parse("08:15:23.500")
      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("make_timestamp") {
      val query      = select(MakeTimestamp(2013, 7, 15, 8, 15, 23.5))
      val expected   = LocalDateTime.parse("2013-07-15T08:15:23.500")
      val testResult = execute(query)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("make_timestampz") {
      def runTest(tz: Timestampz) = {
        val query = select(MakeTimestampz(tz))
        for {
          r <- execute(query).runCollect
        } yield r.head
      }

      val expectedRoundTripTimestamp =
        Timestampz.fromZonedDateTime(ZonedDateTime.of(2020, 11, 21, 19, 10, 25, 0, ZoneId.of(ZoneOffset.UTC.getId)))
      (for {
        t1 <- assertZIO(runTest(Timestampz(2013, 7, 15, 8, 15, 23.5)))(
                equalTo(Timestampz.fromZonedDateTime(ZonedDateTime.parse("2013-07-15T08:15:23.5+00:00")))
              )
        t2 <- assertZIO(runTest(Timestampz(2020, 11, 21, 19, 10, 25, "+00:00")))(
                equalTo(expectedRoundTripTimestamp)
              )
        t3 <- assertZIO(runTest(Timestampz(2020, 11, 21, 15, 10, 25, "-04:00")))(equalTo(expectedRoundTripTimestamp))
        t4 <- assertZIO(runTest(Timestampz(2020, 11, 22, 2, 10, 25, "+07:00")))(equalTo(expectedRoundTripTimestamp))
        t5 <- assertZIO(runTest(Timestampz(2020, 11, 21, 12, 10, 25, "-07:00")))(equalTo(expectedRoundTripTimestamp))
      } yield t1 && t2 && t3 && t4 && t5).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("cannot compile a select without from clause if a table source is required") {
      // the following execute only compiles with 'from customers' clause
      execute((select(CharLength(Customers.fName)) from customers))

      // imports for Left and Right are necessary to make the typeCheck macro expansion compile
      // TODO: clean this up when https://github.com/zio/zio/issues/4927 is resolved
      import scala.util.{ Left, Right }
      val dummyUsage = zio.ZIO.succeed((Left(()), Right(())))

      val result = typeCheck("execute((select(CharLength(Customers.fName))).to[Int, Int](identity))")
      assertZIO(dummyUsage *> result)(isLeft)
    }
  ) @@ timeout(5.minutes)
}
