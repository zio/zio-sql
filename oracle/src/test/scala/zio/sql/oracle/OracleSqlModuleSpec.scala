package zio.sql.oracle

import zio.Chunk
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps
import java.util.UUID
import java.time._

object OracleSqlModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._
  import Orders._

  // final case class CustomerRow(id: String, dateOfBirth: LocalDate, firstName: String, lastName: String, verified: Boolean)

  // implicit val customerRowSchema = DeriveSchema.gen[CustomerRow]
  // implicit val customerRowSchema =
  //       Schema.CaseClass5[UUID, LocalDate, String, String, Boolean, CustomerRow](
  //         TypeId.parse("zio.sql.oracle.OracleModuleSpec.CustomerRow"),
  //         Schema.Field(
  //           "id",
  //           Schema.primitive[UUID](zio.schema.StandardType.UUIDType),
  //           get0 = _.id,
  //           set0 = (r, a) => r.copy(id = a)
  //         ),
  //         Schema.Field(
  //           "dateOfBirth",
  //           Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType),
  //           get0 = _.dateOfBirth,
  //           set0 = (r, a) => r.copy(dateOfBirth = a)
  //         ),
  //         Schema.Field(
  //           "firstName",
  //           Schema.primitive[String](zio.schema.StandardType.StringType),
  //           get0 = _.firstName,
  //           set0 = (r, a) => r.copy(firstName = a)
  //         ),
  //         Schema.Field(
  //           "lastName",
  //           Schema.primitive[String](zio.schema.StandardType.StringType),
  //           get0 = _.lastName,
  //           set0 = (r, a) => r.copy(lastName = a)
  //         ),
  //         Schema.Field(
  //           "verified",
  //           Schema.primitive[Boolean](zio.schema.StandardType.BoolType),
  //           get0 = _.verified,
  //           set0 = (r, a) => r.copy(verified = a)
  //         ),
  //         CustomerRow.apply
  //       )

  override def specLayered: Spec[SqlDriver with TestConfig with Sized, Exception] = suite("Oracle module")(
    test("`in` clause sequence") {
      import ProductPrices._

      val query = select(productPricesOrderId).from(productPrices).where(productPrice in List(10, 20, 74))

      for {
        result <- execute(query).runCollect
      } yield assertTrue(
        result == Chunk(
          UUID.fromString("7368abf4-aed2-421f-b426-1725de756895"),
          UUID.fromString("4c770002-4c8f-455a-96ff-36a8186d5290"),
          UUID.fromString("105a2701-ef93-4e25-81ab-8952cc7d9daa")
        )
      )
    } @@ ignore, // TODO fix Expr.In translation in OracleRenderModule
    test("`in` clause from subquery") {
      import ProductPrices._
      import OrderDetailsSchema._

      val higherPrices = select(productPrice).from(productPrices).where(productPrice > 74)

      val query = select(orderDetailsOrderId).from(orderDetails).where(unitPrice in higherPrices)

      for {
        result <- execute(query).runCollect
      } yield assertTrue(
        result == Chunk(
          UUID.fromString("9473a0bc-396a-4936-96b0-3eea922af36b"),
          UUID.fromString("fd0fa8d4-e1a0-4369-be07-945450db5d36"),
          UUID.fromString("763a7c39-833f-4ee8-9939-e80dfdbfc0fc")
        )
      )
    } @@ ignore, // TODO fix Expr.In translation in OracleRenderModule
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

      val rows = List(
        Customers.Customers(UUID.randomUUID(), LocalDate.ofYearDay(2001, 8), "Peter", "Parker", true),
        Customers.Customers(UUID.randomUUID(), LocalDate.ofYearDay(1980, 2), "Stephen", "Strange", false)
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
    }
    // TODO uncomment and fix java.sql.SQLDataException: ORA-01874: time zone hour must be between -15 and 15
    // oracle won't run on m1 machine
    // test("Can insert all supported types") {
    //   val sqlMinDateTime = LocalDateTime.of(1, 1, 1, 0, 0)
    //   val sqlMaxDateTime = LocalDateTime.of(9999, 12, 31, 23, 59)

    //   val sqlInstant =
    //     Gen.instant(sqlMinDateTime.toInstant(ZoneOffset.MIN), sqlMaxDateTime.toInstant(ZoneOffset.MAX))

    //   val sqlYear = Gen.int(1, 9999).map(Year.of)

    //   val sqlLocalDate = for {
    //     year  <- sqlYear
    //     month <- Gen.int(1, 12)
    //     maxLen = if (!year.isLeap && month == 2) 28 else Month.of(month).maxLength
    //     day   <- Gen.int(1, maxLen)
    //   } yield LocalDate.of(year.getValue, month, day)

    //   val sqlLocalDateTime =
    //     Gen.localDateTime(sqlMinDateTime, sqlMaxDateTime)

    //   val sqlZonedDateTime = for {
    //     dateTime <- sqlLocalDateTime
    //     zoneId   <- Gen.zoneId
    //   } yield ZonedDateTime.of(dateTime, zoneId)

    //   val sqlOffsetTime =
    //     Gen.offsetTime.filter(_.getOffset.getTotalSeconds % 60 == 0)

    //   val sqlOffsetDateTime =
    //     Gen
    //       .offsetDateTime(sqlMinDateTime.atOffset(ZoneOffset.MIN), sqlMaxDateTime.atOffset(ZoneOffset.MAX))
    //       .filter(_.getOffset.getTotalSeconds % 60 == 0)

    //   val javaBigDecimalGen =
    //     for {
    //       bd <- Gen.bigDecimal(Long.MinValue, Long.MaxValue)
    //     } yield bd.bigDecimal

    //   val gen = (
    //     Gen.uuid,
    //     Gen.chunkOf(Gen.byte),
    //     javaBigDecimalGen,
    //     Gen.boolean,
    //     Gen.alphaChar,
    //     Gen.double,
    //     Gen.float,
    //     sqlInstant,
    //     Gen.int,
    //     Gen.option(Gen.int),
    //     sqlLocalDate,
    //     sqlLocalDateTime,
    //     Gen.localTime,
    //     Gen.long,
    //     sqlOffsetDateTime,
    //     sqlOffsetTime,
    //     Gen.short,
    //     Gen.alphaNumericString,
    //     Gen.uuid,
    //     sqlZonedDateTime
    //   ).tupleN

    //   check(gen) { row =>
    //     val insert = insertInto(allTypes)(
    //       id,
    //       bytearrayCol,
    //       bigdecimalCol,
    //       booleanCol,
    //       charCol,
    //       doubleCol,
    //       floatCol,
    //       instantCol,
    //       intCol,
    //       optionalIntCol,
    //       localdateCol,
    //       localdatetimeCol,
    //       localtimeCol,
    //       longCol,
    //       offsetdatetimeCol,
    //       offsettimeCol,
    //       shortCol,
    //       stringCol,
    //       uuidCol,
    //       zonedDatetimeCol
    //     ).values(row)

    //     // printInsert(insert)
    //     // TODO: ensure we can read values back correctly
    //     // val read =
    //     //   select(
    //     //     id ++
    //     //     bytearrayCol ++
    //     //     bigdecimalCol ++
    //     //     booleanCol ++
    //     //     charCol ++
    //     //     doubleCol ++
    //     //     floatCol ++
    //     //     instantCol ++
    //     //     intCol ++
    //     //     optionalIntCol ++
    //     //     localdateCol ++
    //     //     localdatetimeCol ++
    //     //     localtimeCol ++
    //     //     longCol ++
    //     //     offsetdatetimeCol ++
    //     //     offsettimeCol ++
    //     //     shortCol ++
    //     //     stringCol ++
    //     //     uuidCol ++
    //     //     zonedDatetimeCol ++
    //     //     yearMonthCol ++
    //     //     durationCol
    //     //   ).from(allTypes)

    //     val delete = deleteFrom(allTypes).where(id === row._1)

    //     for {
    //       _ <- execute(insert)
    //       // result <- execute(read).runHead
    //       _ <- execute(delete)
    //       // } yield assert(result)(isSome(equalTo(row)))
    //     } yield assertCompletes

    //   }
    // } @@ samples(1) @@ retries(0) @@ shrinks(0)
  ) @@ sequential
}
