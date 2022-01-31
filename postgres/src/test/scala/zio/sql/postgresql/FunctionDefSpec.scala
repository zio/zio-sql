package zio.sql.postgresql

import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID
import zio.{ Cause, Chunk }
import zio.random.{ Random => ZioRandom }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.{ ignore, timeout }
import zio.duration._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import Customers._
  import FunctionDef.{ CharLength => _, _ }
  import PostgresFunctionDef._
  import PostgresSpecific._

  private def collectAndCompare[R, E](
    expected: Seq[String],
    testResult: ZStream[R, E, String]
  ) = {
    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r.toList)(equalTo(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

  private val timestampFormatter = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSSS").withZone(ZoneId.of("UTC"))

  override def specLayered = suite("Postgres FunctionDef")(
    testM("concat_ws #1 - combine flat values") {
      import Expr._

      //note: a plain number (3) would and should not compile
      val query = select(ConcatWs4("+", "1", "2", "3")) from customers

      val expected = Seq( // note: one for each row
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3"
      )

      val testResult = execute(query.to(identity))
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #2 - combine columns") {
      import Expr._

      // note: you can't use customerId here as it is a UUID, hence not a string in our book
      val query = select(ConcatWs3(Customers.fName, Customers.fName, Customers.lName)) from customers

      val expected = Seq(
        "RonaldRonaldRussell",
        "TerrenceTerrenceNoel",
        "MilaMilaPaterso",
        "AlanaAlanaMurray",
        "JoseJoseWiggins"
      )

      val testResult = execute(query.to(identity))
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #3 - combine columns and flat values") {
      import Expr._

      val query = select(ConcatWs4(" ", "Person:", Customers.fName, Customers.lName)) from customers

      val expected = Seq(
        "Person: Ronald Russell",
        "Person: Terrence Noel",
        "Person: Mila Paterso",
        "Person: Alana Murray",
        "Person: Jose Wiggins"
      )

      val testResult = execute(query.to(identity))
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #3 - combine function calls together") {
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

      val testResult = execute(query.to(identity))
      collectAndCompare(expected, testResult)
    },
    testM("isfinite") {
      val query = select(IsFinite(Instant.now))

      val expected: Boolean = true

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    suite("String functions")(
      testM("CharLength") {
        val query    = select(Length("hello"))
        val expected = 5

        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      testM("ltrim") {
        val query = select(Ltrim("  hello  "))

        val expected = "hello  "

        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      testM("rtrim") {
        val query = select(Rtrim("  hello  "))

        val expected = "  hello"

        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      testM("bit_length") {
        val query = select(BitLength("hello"))

        val expected = 40

        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      testM("pi") {
        val query = select(Pi)

        val expected = 3.141592653589793

        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect
        } yield assert(r.head)(equalTo(expected))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      suite("format function")(
        testM("format0") {
          import Expr._

          val query = select(Format0("Person")) from customers

          val expected = Seq(
            "Person",
            "Person",
            "Person",
            "Person",
            "Person"
          )

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        },
        testM("format1") {
          import Expr._

          val query = select(Format1("Person: %s", Customers.fName)) from customers

          val expected = Seq(
            "Person: Ronald",
            "Person: Terrence",
            "Person: Mila",
            "Person: Alana",
            "Person: Jose"
          )

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        },
        testM("format2") {
          import Expr._

          val query = select(Format2("Person: %s %s", Customers.fName, Customers.lName)) from customers

          val expected = Seq(
            "Person: Ronald Russell",
            "Person: Terrence Noel",
            "Person: Mila Paterso",
            "Person: Alana Murray",
            "Person: Jose Wiggins"
          )

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        },
        testM("format3") {
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

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        },
        testM("format4") {
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

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        },
        testM("format5") {
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

          val testResult = execute(query.to(identity))
          collectAndCompare(expected, testResult)
        }
      )
    ),
    testM("abs") {
      val query = select(Abs(-3.14159))

      val expected = 3.14159

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("log") {
      val query = select(Log(2.0, 32.0))

      val expected: Double = 5

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("acos") {
      val query = select(Acos(-1.0))

      val expected = 3.141592653589793

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("repeat") {
      val query = select(Repeat("Zio", 3))

      val expected = "ZioZioZio"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("asin") {
      val query = select(Asin(0.5))

      val expected = 0.5235987755982989

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ln") {
      val query = select(Ln(3.0))

      val expected = 1.0986122886681097

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("atan") {
      val query = select(Atan(10.0))

      val expected = 1.4711276743037347

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("reverse") {
      val query = select(Reverse("abcd"))

      val expected = "dcba"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cos") {
      val query = select(Cos(3.141592653589793))

      val expected = -1.0

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("exp") {
      val query = select(Exp(1.0))

      val expected = 2.718281828459045

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("floor") {
      val query = select(Floor(-3.14159))

      val expected = -4.0

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ceil") {
      val query = select(Ceil(53.7) ++ Ceil(-53.7))

      val expected = (54.0, -53.0)

      val testResult = execute(query.to(value => (value._1, value._2)))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sin") {
      val query = select(Sin(1.0))

      val expected = 0.8414709848078965

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sind") {
      val query = select(Sind(30.0))

      val expected = 0.5

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("split_part") {
      val query = select(SplitPart("abc~@~def~@~ghi", "~@~", 2))

      val expected = "def"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("timeofday") {
      val query = select(TimeOfDay())

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(r.head)(
          matchesRegex(
            "[A-Za-z]{3}\\s[A-Za-z]{3}\\s[0-9]{2}\\s(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9].[0-9]{6}\\s[0-9]{4}\\s[A-Za-z]{3,4}"
          )
        )
      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtime") {
      val query = select(Localtime)

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head.toString)(Assertion.matchesRegex("([0-9]{2}):[0-9]{2}:[0-9]{2}\\.[0-9]{3}"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtime with precision") {
      val precision = 0
      val query     = select(LocaltimeWithPrecision(precision))

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head.toString)(Assertion.matchesRegex(s"([0-9]{2}):[0-9]{2}:[0-9].[0-9]{$precision}"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtimestamp") {
      val query = select(Localtimestamp)

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("localtimestamp with precision") {
      val precision = 2

      val query = select(LocaltimestampWithPrecision(precision))

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex(s"[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}00")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("now") {
      val query = select(Now())

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("statement_timestamp") {
      val query = select(StatementTimestamp())

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("transaction_timestamp") {
      val query = select(TransactionTimestamp())

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(timestampFormatter.format(r.head))(
          Assertion.matchesRegex("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{4}")
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("current_time") {
      val query = select(CurrentTime)

      val testResult = execute(query.to(identity))

      val assertion =
        for {
          r <- testResult.runCollect
        } yield assert(DateTimeFormatter.ofPattern("HH:mm:ss").format(r.head))(
          matchesRegex(
            "(2[0-3]|[01][0-9]):[0-5][0-9]:[0-5][0-9]"
          )
        )

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("md5") {
      val query = select(Md5("hello, world!"))

      val expected = "3adbbad1791fbae3ec908894c4963870"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    suite("parseIdent")(
      testM("parseIdent removes quoting of individual identifiers") {
        val someString: Gen[ZioRandom with Sized, String]    = Gen.anyString
          .filter(x => x.length < 50 && x.length > 1)
        //NOTE: I don't know if property based testing is worth doing here, I just wanted to try it
        val genTestString: Gen[ZioRandom with Sized, String] =
          for {
            string1 <- someString
            string2 <- someString
          } yield s""""${string1}".${string2}"""

        val assertion = checkM(genTestString) { (testString) =>
          val query      = select(ParseIdent(testString))
          val testResult = execute(query.to(identity))

          for {
            r <- testResult.runCollect
          } yield assert(r.head)(not(containsString("'")) && not(containsString("\"")))

        }
        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      testM("parseIdent fails with invalid identifier") {
        val query      = select(ParseIdent("\'\"SomeSchema\".someTable.\'"))
        val testResult = execute(query.to(identity))

        val assertion = for {
          r <- testResult.runCollect.run
        } yield assert(r)(fails(anything))

        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      }
    ) @@ ignore,
    testM("sqrt") {
      val query = select(Sqrt(121.0))

      val expected = 11.0

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("chr") {
      val query = select(Chr(65))

      val expected = "A"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("current_date") {
      val query = select(CurrentDate)

      val expected = LocalDate.now()

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("initcap") {
      val query = select(Initcap("hi THOMAS"))

      val expected = "Hi Thomas"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("trim_scale") {
      val query = select(TrimScale(8.4100))

      val expected = 8.41

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("hex") {
      val query = select(Hex(2147483647))

      val expected = "7fffffff"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("encode") {
      val query = select(Encode(Chunk.fromArray("Hello, World!".getBytes), "BASE64"))

      val expected = "SGVsbG8sIFdvcmxkIQ=="

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("decode") {
      val query = select(Decode("SGVsbG8sIFdvcmxkIQ==", "BASE64"))

      val expected = Chunk.fromArray("Hello, World!".getBytes)

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("trunc") {
      val query = select(Trunc(42.8))

      val expected = 42d

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("round") {
      val query = select(Round(10.8124, 2))

      val expected = 10.81

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign positive") {
      val query = select(Sign(3.0))

      val expected = 1

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign negative") {
      val query = select(Sign(-3.0))

      val expected = -1

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign zero") {
      val query = select(Sign(0.0))

      val expected = 0

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("power") {
      val query = select(Power(7.0, 3.0))

      val expected = 343.000000000000000

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("length") {
      val query = select(Length("hello"))

      val expected = 5

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("mod") {
      val query = select(Mod(-15.0, -4.0))

      val expected = -3.0

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("translate") {
      val query = select(Translate("12345", "143", "ax"))

      val expected = "a2x5"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("left") {
      val query = select(Left("abcde", 2))

      val expected = "ab"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("right") {
      val query = select(Right("abcde", 2))

      val expected = "de"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("radians") {
      val query = select(Radians(45.0))

      val expected = 0.7853981634

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(approximatelyEquals(expected, 10.0))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("min_scale") {
      val query = select(MinScale(8.4100))

      val expected = 2

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("starts_with") {
      case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query =
        (select(customerId ++ fName ++ lName ++ verified ++ dob) from customers).where(StartsWith(fName, "R"))

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

      val testResult = execute(
        query.to { case (id, fname, lname, verified, dob) =>
          Customer(id, fname, lname, verified, dob)
        }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lower") {
      val query = select(Lower(fName)) from customers limit (1)

      val expected = "ronald"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lower with string literal") {
      val query = select(Lower("LOWER")) from customers limit (1)

      val expected = "lower"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("octet_length") {
      val query = select(OctetLength("josé"))

      val expected = 5

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ascii") {
      val query = select(Ascii("""x"""))

      val expected = 120

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("upper") {
      val query = (select(Upper("ronald"))).limit(1)

      val expected = "RONALD"

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("width_bucket") {
      val query = select(WidthBucket(5.35, 0.024, 10.06, 5))

      val expected = 3

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("tan") {
      val query = select(Tan(0.7853981634))

      val expected = 1.0000000000051035

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("gcd") {
      val query = select(GCD(1071d, 462d))

      val expected = 21d

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lcm") {
      val query = select(LCM(1071d, 462d))

      val expected = 23562d

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cbrt") {
      val query = select(CBRT(64.0))

      val expected = 4d

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("degrees") {
      val query = select(Degrees(0.5))

      val expected = 28.64788975654116

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("div") {
      val query = select(Div(8d, 4d))

      val expected = 2d

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("factorial") {
      val query = select(Factorial(5))

      val expected = 120

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("random") {
      val query = select(Random())

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(Assertion.isGreaterThanEqualTo(0d) && Assertion.isLessThanEqualTo(1d))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("setseed") {
      val query = select(SetSeed(0.12) ++ Random() ++ Random()) from customers

      val randomTupleForSeed = (0.019967750719779076, 0.8378369929936333)
      val testResult         = execute(query.to { case (_, b, c) => (b, c) })

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.take(2))(equalTo(Chunk(randomTupleForSeed, randomTupleForSeed)))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can concat strings with concat function") {

      val query = select(Concat(fName, lName) as "fullname") from customers

      val expected = Seq("RonaldRussell", "TerrenceNoel", "MilaPaterso", "AlanaMurray", "JoseWiggins")

      val result = execute(query.to(identity))

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can calculate character length of a string") {

      val query = select(CharLength(fName)) from customers

      val expected = Seq(6, 8, 4, 5, 4)

      val result = execute(query.to(identity))

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElements(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("to_timestamp") {
      val query      = select(ToTimestamp(1284352323L))
      val expected   = ZonedDateTime.of(2010, 9, 13, 4, 32, 3, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val testResult = execute(query.to(identity))

      val expectedRoundTripTimestamp = ZonedDateTime.of(2020, 11, 21, 19, 10, 25, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val roundTripQuery             =
        select(createdString ++ createdTimestamp) from customers
      val roundTripResults           = execute(roundTripQuery.to { case row =>
        (row._1, ZonedDateTime.parse(row._1), row._2)
      })
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
    testM("replace") {
      val lastNameReplaced = Replace(lName, "ll", "_") as "lastNameReplaced"
      val computedReplace  = Replace("special ::ąę::", "ąę", "__") as "computedReplace"

      val query = select(lastNameReplaced ++ computedReplace) from customers

      val expected = ("Russe_", "special ::__::")

      val testResult =
        execute(query.to { case row =>
          (row._1, row._2)
        })

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lpad") {
      def runTest(s: String, pad: String) = {
        val query = select(LPad(s, 5, pad))

        for {
          r <- execute(query.to(identity)).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertM(runTest("hi", "xy"))(equalTo("xyxhi"))
        t2 <- assertM(runTest("hello", "xy"))(equalTo("hello"))
        t3 <- assertM(runTest("hello world", "xy"))(equalTo("hello"))
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("rpad") {
      def runTest(s: String, pad: String) = {
        val query = select(RPad(s, 5, pad))

        for {
          r <- execute(query.to(identity)).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertM(runTest("hi", "xy"))(equalTo("hixyx"))
        t2 <- assertM(runTest("hello", "xy"))(equalTo("hello"))
        t3 <- assertM(runTest("hello world", "xy"))(equalTo("hello"))
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("pg_client_encoding") {
      val query = select(PgClientEncoding())

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo("UTF8"))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
      @@ ignore, //todo fix - select(PgClientEncoding())?
    testM("make_date") {
      val query = select(MakeDate(2013, 7, 15))

      val expected = LocalDate.of(2013, 7, 15)

      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("make_interval") {
      def runTest(interval: Interval) = {
        val query = select(
          MakeInterval(interval)
        )
        for {
          r <- execute(query.to(identity)).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertM(runTest(Interval()))(equalTo(Interval()))
        t2 <- assertM(runTest(Interval(days = 10)))(equalTo(Interval(days = 10)))
        t3 <- assertM(
                runTest(Interval(years = 10, months = 2, days = 5, hours = 6, minutes = 20, seconds = 15))
              )(
                equalTo(Interval(years = 10, months = 2, days = 5, hours = 6, minutes = 20, seconds = 15))
              )
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("make_time") {
      val query      = select(MakeTime(8, 15, 23.5))
      val expected   = LocalTime.parse("08:15:23.500")
      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("make_timestamp") {
      val query      = select(MakeTimestamp(2013, 7, 15, 8, 15, 23.5))
      val expected   = LocalDateTime.parse("2013-07-15T08:15:23.500")
      val testResult = execute(query.to(identity))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("make_timestampz") {
      def runTest(tz: Timestampz) = {
        val query = select(MakeTimestampz(tz))
        for {
          r <- execute(query.to(identity)).runCollect
        } yield r.head
      }

      val expectedRoundTripTimestamp =
        Timestampz.fromZonedDateTime(ZonedDateTime.of(2020, 11, 21, 19, 10, 25, 0, ZoneId.of(ZoneOffset.UTC.getId)))
      (for {
        t1 <- assertM(runTest(Timestampz(2013, 7, 15, 8, 15, 23.5)))(
                equalTo(Timestampz.fromZonedDateTime(ZonedDateTime.parse("2013-07-15T08:15:23.5+00:00")))
              )
        t2 <- assertM(runTest(Timestampz(2020, 11, 21, 19, 10, 25, "+00:00")))(
                equalTo(expectedRoundTripTimestamp)
              )
        t3 <- assertM(runTest(Timestampz(2020, 11, 21, 15, 10, 25, "-04:00")))(equalTo(expectedRoundTripTimestamp))
        t4 <- assertM(runTest(Timestampz(2020, 11, 22, 2, 10, 25, "+07:00")))(equalTo(expectedRoundTripTimestamp))
        t5 <- assertM(runTest(Timestampz(2020, 11, 21, 12, 10, 25, "-07:00")))(equalTo(expectedRoundTripTimestamp))
      } yield t1 && t2 && t3 && t4 && t5).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cannot compile a select without from clause if a table source is required") {
      //the following execute only compiles with 'from customers' clause
      execute((select(CharLength(Customers.fName)) from customers).to(identity))

      // imports for Left and Right are necessary to make the typeCheck macro expansion compile
      // TODO: clean this up when https://github.com/zio/zio/issues/4927 is resolved
      import scala.util.Right
      import scala.util.Left
      val dummyUsage = zio.ZIO.succeed((Left(()), Right(())))

      val result = typeCheck("execute((select(CharLength(Customers.fName))).to[Int, Int](identity))")
      assertM(dummyUsage *> result)(isLeft)
    }
  ) @@ timeout(5.minutes)
}
