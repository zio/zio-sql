package zio.sql.mysql

import java.time._
import java.util.UUID
import zio._
import zio.schema._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import java.math.BigDecimal
import zio.sql.table._
import scala.language.postfixOps
import zio.sql.expr.AggregationDef._

object MysqlModuleSpec extends MysqlRunnableSpec {

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = Table.defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  case class Orders(id: UUID, customer_id: UUID, order_date: LocalDate, deleted_at: Option[LocalDateTime])

  implicit val orderSchema = DeriveSchema.gen[Orders]

  val orders = Table.defineTable[Orders]

  val (orderId, fkCustomerId, orderDate, deletedAt) = orders.columns

  case class ProductPrice(productId: UUID, effective: LocalDate, price: BigDecimal)
  implicit val productPriceSchema = DeriveSchema.gen[ProductPrice]

  val productPrices = Table.defineTableSmart[ProductPrice]

  val (productPricesOrderId, effectiveDate, productPrice) = productPrices.columns

  case class OrderDetails(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

  implicit val orderDetailsSchema = DeriveSchema.gen[OrderDetails]

  val orderDetails = Table.defineTableSmart[OrderDetails]

  val (orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice) = orderDetails.columns

  override def specLayered = suite("Mysql module")(
    test("`in` clause sequence") {
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
    },
    test("`in` clause from subquery") {
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
    },
    test("Can select from single table") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob) from customers

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
          ),
          Customer(
            UUID.fromString("d4f6c156-20ac-4d27-8ced-535bf4315ebc"),
            "Robert",
            "Rupert",
            LocalDate.parse("1998-06-11")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4)
      }

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
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

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
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

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Execute union on select queries") {
      val query = select(customerId).from(customers).union(select(fkCustomerId).from(orders))

      val expected =
        Seq(
          UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
          UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"),
          UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"),
          UUID.fromString("d4f6c156-20ac-4d27-8ced-535bf4315ebc"),
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
          UUID.fromString("d4f6c156-20ac-4d27-8ced-535bf4315ebc"),
          UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc")
        )

      assertZIO(execute(query).runCollect)(hasSameElementsDistinct(expected))
    },
    /*
     * This is a failing test for aggregation function.
     * Uncomment it when aggregation function handling is fixed.
     */
    test("Can count rows") {
      val query = select(Count(customerId)) from customers

      for {
        r <- execute(query).runCollect
      } yield assertTrue(r.head == 6L)
    },
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

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
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
          TypeId.parse("zio.sql.mysql.CustomerRow"),
          Schema.Field(
            "id",
            Schema.primitive[UUID](zio.schema.StandardType.UUIDType),
            get0 = _.id,
            set0 = (r, a) => r.copy(id = a)
          ),
          Schema.Field(
            "dateOfBirth",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType),
            get0 = _.dateOfBirth,
            set0 = (r, a) => r.copy(dateOfBirth = a)
          ),
          Schema.Field(
            "firstName",
            Schema.primitive[String](zio.schema.StandardType.StringType),
            get0 = _.firstName,
            set0 = (r, a) => r.copy(firstName = a)
          ),
          Schema.Field(
            "lastName",
            Schema.primitive[String](zio.schema.StandardType.StringType),
            get0 = _.lastName,
            set0 = (r, a) => r.copy(lastName = a)
          ),
          Schema.Field(
            "verified",
            Schema.primitive[Boolean](zio.schema.StandardType.BoolType),
            get0 = _.verified,
            set0 = (r, a) => r.copy(verified = a)
          ),
          CustomerRow.apply
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
        deletedAt
      ).values(rows)

      println(renderInsert(command))

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can update rows") {
      val query = update(customers).set(fName, "Roland").where(fName === "Ronald")

      println(renderUpdate(query))
      assertZIO(execute(query))(equalTo(1))
    }
  ) @@ sequential

}
