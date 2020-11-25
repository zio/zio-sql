package zio.sql.postgresql

import java.time.{ LocalDate, ZoneId, ZoneOffset, ZonedDateTime }
import java.util.UUID
import zio.Cause
import zio.random.{ Random => ZioRandom }
import zio.test.Assertion._
import zio.test._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import Customers._
  import FunctionDef._
  import PostgresFunctionDef._

  val spec = suite("Postgres FunctionDef")(
    testM("abs") {
      val query = select(Abs(-3.14159)) from customers

      val expected = 3.14159

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("log") {
      val query = select(Log(2.0, 32.0)) from customers

      val expected: Double = 5

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("acos") {
      val query = select(Acos(-1.0)) from customers

      val expected = 3.141592653589793

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("repeat") {
      val query = select(Repeat("'Zio'", 3)) from customers

      val expected = "ZioZioZio"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("asin") {
      val query = select(Asin(0.5)) from customers

      val expected = 0.5235987755982989

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ln") {
      val query = select(Ln(3.0)) from customers

      val expected = 1.0986122886681097

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("atan") {
      val query = select(Atan(10.0)) from customers

      val expected = 1.4711276743037347

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("reverse") {
      val query = select(Reverse("'abcd'")) from customers

      val expected = "dcba"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cos") {
      val query = select(Cos(3.141592653589793)) from customers

      val expected = -1.0

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("exp") {
      val query = select(Exp(1.0)) from customers

      val expected = 2.718281828459045

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("floor") {
      val query = select(Floor(-3.14159)) from customers

      val expected = -4.0

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ceil") {
      val query = select(Ceil(53.7) ++ Ceil(-53.7)) from customers

      val expected = (54.0, -53.0)

      val testResult = execute(query).to[Double, Double, (Double, Double)]((a, b) => (a, b))

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
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
    testM("md5") {
      val query = select(Md5("'hello, world!'")) from customers

      val expected = "3adbbad1791fbae3ec908894c4963870"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("parseIdent removes quoting of individual identifiers") {
      val someString: Gen[ZioRandom with Sized, String]    = Gen.anyString
        .filter(x => x.length < 50 && x.length > 1)
      //NOTE: I don't know if property based testing is worth doing here, I just wanted to try it
      val genTestString: Gen[ZioRandom with Sized, String] =
        for {
          string1 <- someString
          string2 <- someString
        } yield s"""'"${string1}".${string2}'"""

      val assertion = checkM(genTestString) { (testString) =>
        val query      = select(ParseIdent(testString)) from customers
        val testResult = execute(query).to[String, String](identity)

        for {
          r <- testResult.runCollect
        } yield assert(r.head)(not(containsString("'")) && not(containsString("\"")))

      }
      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("parseIdent fails with invalid identifier") {
      val query      = select(ParseIdent("\'\"SomeSchema\".someTable.\'")) from customers
      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect.run
      } yield assert(r)(fails(anything))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sqrt") {
      val query = select(Sqrt(121.0)) from customers

      val expected = 11.0

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("chr") {
      val query = select(Chr(65)) from customers

      val expected = "A"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("current_date") {
      val query = select(CurrentDate()) from customers

      val expected = LocalDate.now()

      val testResult = execute(query).to[LocalDate, LocalDate](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("initcap") {
      val query = select(Initcap("'hi THOMAS'")) from customers

      val expected = "Hi Thomas"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("trim_scale") {
      val query = select(TrimScale(8.4100)) from customers

      val expected = 8.41

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("hex") {
      val query = select(Hex(2147483647)) from customers

      val expected = "7fffffff"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("trunc") {
      val query = select(Trunc(42.8)) from customers

      val expected = 42d

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("round") {
      val query = select(Round(10.8124, 2)) from customers

      val expected = 10.81

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign positive") {
      val query = select(Sign(3.0)) from customers

      val expected = 1

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign negative") {
      val query = select(Sign(-3.0)) from customers

      val expected = -1

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sign zero") {
      val query = select(Sign(0.0)) from customers

      val expected = 0

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("power") {
      val query = select(Power(7.0, 3.0)) from customers

      val expected = 343.000000000000000

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("length") {
      val query = select(Length("'hello'")) from customers

      val expected = 5

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("mod") {
      val query = select(Mod(-15.0, -4.0)) from customers

      val expected = -3.0

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("translate") {
      val query = select(Translate("'12345'", "'143'", "'ax'")) from customers

      val expected = "a2x5"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("left") {
      val query = select(Left("'abcde'", 2)) from customers

      val expected = "ab"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("right") {
      val query = select(Right("'abcde'", 2)) from customers

      val expected = "de"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("radians") {
      val query = select(Radians(45.0)) from customers

      val expected = 0.7853981634

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(approximatelyEquals(expected, 10.0))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("min_scale") {
      val query = select(MinScale(8.4100)) from customers

      val expected = 2

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("starts_with") {
      case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query =
        (select(customerId ++ fName ++ lName ++ verified ++ dob) from customers).where(StartsWith(fName, """'R'"""))

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

      val testResult = execute(query)
        .to[UUID, String, String, Boolean, LocalDate, Customer]((id, fname, lname, verified, dob) =>
          Customer(id, fname, lname, verified, dob)
        )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lower") {
      val query = select(Lower("first_name")) from customers limit (1)

      val expected = "ronald"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("octet_length") {
      val query = select(OctetLength("'josé'")) from customers

      val expected = 5

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("ascii") {
      val query = select(Ascii("""'x'""")) from customers

      val expected = 120

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("upper") {
      val query = (select(Upper("first_name")) from customers).limit(1)

      val expected = "RONALD"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("width_bucket") {
      val query = select(WidthBucket(5.35, 0.024, 10.06, 5)) from customers

      val expected = 3

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("tan") {
      val query = select(Tan(0.7853981634)) from customers

      val expected = 1.0000000000051035

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("gcd") {
      val query = select(GCD(1071d, 462d)) from customers

      val expected = 21d

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lcm") {
      val query = select(LCM(1071d, 462d)) from customers

      val expected = 23562d

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cbrt") {
      val query = select(CBRT(64.0)) from customers

      val expected = 4d

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("degrees") {
      val query = select(Degrees(0.5)) from customers

      val expected = 28.64788975654116

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("div") {
      val query = select(Div(8d, 4d)) from customers

      val expected = 2d

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("factorial") {
      val query = select(Factorial(5)) from customers

      val expected = 120

      val testResult = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("random") {
      val query = select(Random()) from customers

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(Assertion.isGreaterThanEqualTo(0d) && Assertion.isLessThanEqualTo(1d))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can concat strings with concat function") {

      val query = select(Concat("first_name", "last_name") as "fullname") from customers

      val expected = Seq("RonaldRussell", "TerrenceNoel", "MilaPaterso", "AlanaMurray", "JoseWiggins")

      val result = execute(query).to[String, String](identity)

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can calculate character length of a string") {

      val query = select(CharLength("first_name")) from customers

      val expected = Seq(6, 8, 4, 5, 4)

      val result = execute(query).to[Int, Int](identity)

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElements(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("to_timestamp") {
      val query      = select(ToTimestamp(1284352323L)) from customers
      val expected   = ZonedDateTime.of(2010, 9, 13, 4, 32, 3, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val testResult = execute(query).to[ZonedDateTime, ZonedDateTime](identity)

      val expectedRoundTripTimestamp = ZonedDateTime.of(2020, 11, 21, 19, 10, 25, 0, ZoneId.of(ZoneOffset.UTC.getId))
      val roundTripQuery             =
        select(createdString ++ createdTimestamp) from customers
      val roundTripResults           = execute(roundTripQuery).to[String, ZonedDateTime, (String, ZonedDateTime, ZonedDateTime)] {
        case row =>
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
    testM("replace") {
      val lastNameReplaced = Replace(lName, "'ll'", "'_'") as "lastNameReplaced"
      val computedReplace  = Replace("'special ::ąę::'", "'ąę'", "'__'") as "computedReplace"

      val query = select(lastNameReplaced ++ computedReplace) from customers

      val expected = ("Russe_", "special ::__::")

      val testResult =
        execute(query).to[String, String, (String, String)] { case row =>
          (row._1, row._2)
        }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("lpad") {
      def runTest(s: String, pad: String) = {
        val query = select(LPad(postgresStringEscape(s), 5, postgresStringEscape(pad))) from customers

        for {
          r <- execute(query).to[String, String](identity).runCollect
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
        val query = select(RPad(postgresStringEscape(s), 5, postgresStringEscape(pad))) from customers

        for {
          r <- execute(query).to[String, String](identity).runCollect
        } yield r.head
      }

      (for {
        t1 <- assertM(runTest("hi", "xy"))(equalTo("hixyx"))
        t2 <- assertM(runTest("hello", "xy"))(equalTo("hello"))
        t3 <- assertM(runTest("hello world", "xy"))(equalTo("hello"))
      } yield t1 && t2 && t3).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )

  private def postgresStringEscape(s: String): String = s""" '${s}' """
}
