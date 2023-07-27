package zio.sql.mysql

import zio.test._
import zio.test.Assertion._
import zio.schema._
import java.time.{ LocalDate, LocalTime, ZoneId }
import java.time.format.DateTimeFormatter
import zio.sql.Jdbc
import java.util.UUID
import zio.sql.table._

object CustomFunctionDefSpec extends MysqlRunnableSpec with Jdbc {

  import MysqlFunctionDef._

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = Table.defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  override def specLayered = suite("MySQL FunctionDef")(
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
    test("soundex outputs should not match for non-similar-sounding strings") {
      val queryForRobert = select(Soundex("Robert"))
      val queryForTam    = select(Soundex("Tam"))

      val resultForRobert = execute(queryForRobert)
      val resultForTam    = execute(queryForTam)

      for {
        robertResult <- resultForRobert.runCollect
        tamResult    <- resultForTam.runCollect
      } yield assert(robertResult.head.equals(tamResult.head))(equalTo(false))
    },
    test("soundex outputs should match for equivalent strings") {
      val queryForRobert = select(Soundex("Robert"))
      val queryForRupert = select(Soundex("Rupert"))

      val resultForRobert = execute(queryForRobert)
      val resultForRupert = execute(queryForRupert)

      for {
        robertResult <- resultForRobert.runCollect
        rupertResult <- resultForRupert.runCollect
      } yield assert(robertResult.head.equals(rupertResult.head))(equalTo(true))
    },
    test("soundex") {
      val query    = select(Soundex("Robert"))
      val expected = "R163"

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
    test("uuid") {
      assertZIO(execute(select(Uuid)).runHead.some)(!isNull)
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
    },
    test("Radians") {
      val query = select(Radians(40d))

      val expected = Math.toRadians(40d)

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    },
    test("makedate") {
      val query = select(MakeDate(2022, 31)) from customers

      val expected = LocalDate.of(2022, 1, 31)

      val testResult = execute(query)

      assertZIO(testResult.runHead.some)(equalTo(expected))
    }
  )
}
