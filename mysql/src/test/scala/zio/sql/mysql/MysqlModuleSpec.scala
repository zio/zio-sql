package zio.sql.mysql

import java.time.LocalDate
import java.util.UUID

import zio.test._
import zio.test.Assertion._
import scala.language.postfixOps
import zio.schema.DeriveSchema

object MysqlModuleSpec extends MysqlRunnableSpec {

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  case class Orders(id: UUID, customer_id: UUID, order_date: LocalDate)

  implicit val orderSchema = DeriveSchema.gen[Orders]

  val orders = defineTable[Orders]

  val (orderId, fkCustomerId, orderDate) = orders.columns

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

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Can select with property operator") {
      case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, verified, dob) from customers where (verified isNotTrue)

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
    test("Can count rows") {
      import AggregationDef._
      val query = select(Count(customerId)) from customers

      for {
        r <- execute(query).runCollect
      } yield assertTrue(r.head == 5L)
    },
    test("Can select from joined tables (inner join)") {
      val (customerId1, _, fName1, lName1, _) = customers.columns

      val query = select(fName1, lName1, orderDate) from (customers join orders).on(
        fkCustomerId === customerId1
      ) where (verified isNotTrue)

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
    test("Can update rows") {
      val query = update(customers).set(fName, "Roland").where(fName === "Ronald")

      for {
        result <- execute(query)
      } yield assertTrue(result == 1)
    }
  )
}
