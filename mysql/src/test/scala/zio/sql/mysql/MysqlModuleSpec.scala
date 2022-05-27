package zio.sql.mysql

import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.language.postfixOps

import zio._
import zio.schema._
import zio.test._
import zio.test.Assertion._

object MysqlModuleSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._
  import Orders._

  override def specLayered = suite("Mysql module")(
    test("Can select from single table") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob) from customers

      println(renderRead(query))

      val expected =
        Seq(
          Customer(
            UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
            "Ronald",
            "Russell",
            LocalDate.parse("1983-01-05")
          ),
          Customer(
            UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"),
            "Terrence",
            "Noel",
            LocalDate.parse("1999-11-02")
          ),
          Customer(
            UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"),
            "Mila",
            "Paterso",
            LocalDate.parse("1990-11-16")
          ),
          Customer(
            UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
            "Alana",
            "Murray",
            LocalDate.parse("1995-11-12")
          ),
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can select with property operator") {
      case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, verified, dob) from customers where (verified isNotTrue)

      println(renderRead(query))

      val expected =
        Seq(
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            false,
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4, row._5)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = (select(customerId, fName, lName, dob) from customers).limit(1).offset(1).orderBy(fName)

      println(renderRead(query))

      val expected =
        Seq(
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Execute union on select queries") {
      val query = select(customerId).from(customers).union(select(fkCustomerId).from(orders))

      val expected =
        Seq(
          UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
          UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"),
          UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"),
          UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc")
        )

      assertZIO(execute(query).runCollect)(hasSameElements(expected))
    },
    test("Execute union all on select queries") {
      val query = select(customerId).from(customers).unionAll(select(fkCustomerId).from(orders))

      val expected =
        Chunk(
          UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
          UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"),
          UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"),
          UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc")
        )

      assertZIO(execute(query).runCollect)(hasSameElementsDistinct(expected))
    },
    /*
     * This is a failing test for aggregation function.
     * Uncomment it when aggregation function handling is fixed.
     */
    // test("Can count rows") {
    //   val query = select { Count(userId) } from users

    //   val expected = 5L

    //   val result = new ExecuteBuilder(query).to[Long, Long](identity).provideCustomLayer(executorLayer)

    //   for {
    //     r <- result.runCollect
    //   } yield assert(r.head)(equalTo(expected))
    // },
    test("Can select from joined tables (inner join)") {
      val query = select(fName, lName, orderDate) from (customers join orders).on(
        fkCustomerId === customerId
      ) where (verified isNotTrue)

      println(renderRead(query))

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(
        Row("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
        Row("Jose", "Wiggins", LocalDate.parse("2020-01-15"))
      )

      val result = execute(query).map { case row =>
        Row(row._1, row._2, row._3)
      }

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
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

      println(renderInsert(command))

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert tuples") {
      implicit val optionLocalDateTimeSchema = Schema.option[LocalDateTime]

      val rows = List(
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 1),
          None
        ),
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 5),
          Some(LocalDateTime.of(2022, 1, 10, 3, 20))
        )
      )

      val command = insertInto(orders)(
        orderId,
        fkCustomerId,
        orderDate,
        deleted_at
      ).values(rows)

      println(renderInsert(command))

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can update rows") {
      val query = update(customers).set(fName, "Roland").where(fName === "Ronald")

      println(renderUpdate(query))
      assertZIO(execute(query))(equalTo(1))
    }
  )

}
