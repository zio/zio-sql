package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps
import java.util.UUID
import java.time.format.DateTimeFormatter
import zio.schema.Schema
import zio.prelude._
import java.time.{ LocalDate, LocalDateTime, Month, Year, YearMonth, ZoneOffset, ZonedDateTime }

object OracleSqlModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._
  import Orders._
  import AllTypes._

  override def specLayered: Spec[SqlDriver with TestConfig with Sized, Exception] = suite("Oracle module")(
    test("Can update selected rows") {

      /**
       * UPDATE customers SET customers.first_name = 'Antek'
       * WHERE 1 = 1 and customers.verified = 0 and customers.verified <> 1
       */
      val query =
        update(customers)
          .set(fName, "Antek")
          .where(verified isNotTrue)
          .where(verified <> true) // we intentionally verify two syntax variants

      assertZIO(execute(query))(equalTo(1))
    },
    test("Can update all rows") {

      /**
       * UPDATE customers SET customers.first_name = 'Antek' WHERE 1 = 1
       */
      val query = update(customers).set(fName, "Antek")

      assertZIO(execute(query))(equalTo(5))
    },
    test("Can delete from single table with a condition") {

      /**
       * DELETE FROM customers WHERE customers.verified = 0
       */
      val query = deleteFrom(customers) where (verified isNotTrue)

      val expected = 1
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    },
    test("Can delete all from a single table") {

      /**
       * DELETE FROM customers
       */
      val query = deleteFrom(customers)

      val expected = 4
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    },
    test("Can insert rows") {
      final case class CustomerRow(
        id: UUID,
        dateOfBirth: LocalDate,
        firstName: String,
        lastName: String,
        verified: Boolean
      )
      implicit val customerRowSchema =
        Schema.CaseClass5[UUID, LocalDate, String, String, Boolean, CustomerRow](
          Schema.Field("id", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
          Schema.Field(
            "dateOfBirth",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))
          ),
          Schema.Field("firstName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("lastName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("verified", Schema.primitive[Boolean](zio.schema.StandardType.BoolType)),
          CustomerRow.apply,
          _.id,
          _.dateOfBirth,
          _.firstName,
          _.lastName,
          _.verified
        )

      val rows = List(
        CustomerRow(UUID.randomUUID(), LocalDate.ofYearDay(2001, 8), "Peter", "Parker", true),
        CustomerRow(UUID.randomUUID(), LocalDate.ofYearDay(1980, 2), "Stephen", "Strange", false)
      )

      val command = insertInto(customers)(
        customerId,
        dob,
        fName,
        lName,
        verified
      ).values(rows)

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert tuples") {

      val rows = List(
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 1)
        ),
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 5)
        )
      )

      val command = insertInto(orders)(
        orderId,
        fkCustomerId,
        orderDate
      ).values(rows)

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert all supported types") {
      val sqlMinDateTime = LocalDateTime.of(-4713, 1, 1, 0, 0)
      val sqlMaxDateTime = LocalDateTime.of(9999, 12, 31, 23, 59)

      val sqlInstant =
        Gen.instant(sqlMinDateTime.toInstant(ZoneOffset.MIN), sqlMaxDateTime.toInstant(ZoneOffset.MAX))

      val sqlYear = Gen.int(-4713, 9999).filter(_ != 0).map(Year.of)

      val sqlLocalDate = for {
        year  <- sqlYear
        month <- Gen.int(1, 12)
        maxLen = if (!year.isLeap && month == 2) 28 else Month.of(month).maxLength
        day   <- Gen.int(1, maxLen)
      } yield LocalDate.of(year.getValue, month, day)

      val sqlYearMonth = for {
        year  <- sqlYear
        month <- Gen.int(1, 12)
      } yield YearMonth.of(year.getValue(), month)

      val sqlLocalDateTime =
        Gen.localDateTime(sqlMinDateTime, sqlMaxDateTime)

      val sqlZonedDateTime = for {
        dateTime <- sqlLocalDateTime
        zoneId   <- Gen.zoneId
      } yield ZonedDateTime.of(dateTime, zoneId)

      val sqlOffsetTime =
        Gen.offsetTime.filter(_.getOffset.getTotalSeconds % 60 == 0)

      val sqlOffsetDateTime =
        Gen
          .offsetDateTime(sqlMinDateTime.atOffset(ZoneOffset.MIN), sqlMaxDateTime.atOffset(ZoneOffset.MAX))
          .filter(_.getOffset.getTotalSeconds % 60 == 0)

      val gen = (
        Gen.uuid,
        Gen.chunkOf(Gen.byte),
        Gen.bigDecimal(Long.MinValue, Long.MaxValue),
        Gen.boolean,
        Gen.char,
        Gen.double,
        Gen.float,
        sqlInstant,
        Gen.int,
        Gen.option(Gen.int),
        sqlLocalDate,
        sqlLocalDateTime,
        Gen.localTime,
        Gen.long,
        sqlOffsetDateTime,
        sqlOffsetTime,
        Gen.short,
        Gen.string,
        Gen.uuid,
        sqlZonedDateTime,
        sqlYearMonth,
        Gen.finiteDuration
      ).tupleN
      check(gen) { row =>
        val insert = insertInto(allTypes)(
          id,
          bytearrayCol,
          bigdecimalCol,
          booleanCol,
          charCol,
          doubleCol,
          floatCol,
          instantCol,
          intCol,
          optionalIntCol,
          localdateCol,
          localdatetimeCol,
          localtimeCol,
          longCol,
          offsetdatetimeCol,
          offsettimeCol,
          shortCol,
          stringCol,
          uuidCol,
          zonedDatetimeCol,
          yearMonthCol,
          durationCol
        ).values(row)

        // TODO: ensure we can read values back correctly
        // val read =
        //   select(
        //     id ++
        //     bytearrayCol ++
        //     bigdecimalCol ++
        //     booleanCol ++
        //     charCol ++
        //     doubleCol ++
        //     floatCol ++
        //     instantCol ++
        //     intCol ++
        //     optionalIntCol ++
        //     localdateCol ++
        //     localdatetimeCol ++
        //     localtimeCol ++
        //     longCol ++
        //     offsetdatetimeCol ++
        //     offsettimeCol ++
        //     shortCol ++
        //     stringCol ++
        //     uuidCol ++
        //     zonedDatetimeCol ++
        //     yearMonthCol ++
        //     durationCol
        //   ).from(allTypes)

        val delete = deleteFrom(allTypes).where(id === row._1)

        for {
          _ <- execute(insert)
          // result <- execute(read).runHead
          _ <- execute(delete)
          // } yield assert(result)(isSome(equalTo(row)))
        } yield assertCompletes

      }
    } @@ samples(1) @@ retries(0) @@ shrinks(0)
  ) @@ sequential
}
