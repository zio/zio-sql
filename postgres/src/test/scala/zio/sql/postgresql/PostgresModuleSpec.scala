package zio.sql.postgresql

import java.time.LocalDate
import java.util.UUID

import zio._
import zio.test.Assertion._
import zio.test._

import scala.language.postfixOps

object PostgresModuleSpec extends PostgresRunnableSpec with ShopSchema {

  import Customers._
  import Orders._

  private def customerSelectJoseAssertion(condition: Expr[_, customers.TableType, Boolean]) = {
    case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

    val query =
      select(customerId ++ fName ++ lName ++ verified ++ dob) from customers where (condition)

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

    val testResult = execute(
      query
        .to[UUID, String, String, Boolean, LocalDate, Customer] { case row =>
          Customer(row._1, row._2, row._3, row._4, row._5)
        }
    )

    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r)(hasSameElementsDistinct(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

  override def specLayered = suite("Postgres module")(
    testM("Can select from single table") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob) from customers

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

//      execute(query ++ query ++ query ++ query)

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select with property unary operator") {
      customerSelectJoseAssertion(verified isNotTrue)
    },
// TODO: uncomment when #311 (rendering literals) will be fixed
//    testM("Can select with property binary operator with UUID") {
//      customerSelectJoseAssertion(customerId === UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"))
//    },
//    testM("Can select with property binary operator with String") {
//      customerSelectJoseAssertion(fName === "Jose")
//    },
//    testM("Can select with property binary operator with LocalDate") {
//      customerSelectJoseAssertion(dob === LocalDate.parse("1987-03-23"))
//    },
//    testM("Can select with property binary operator with LocalDateTime") {
//      customerSelectJoseAssertion(dob === LocalDateTime.parse("1987-03-23T00:00:00"))
//    },
//    testM("Can select with property binary operator with OffsetDateTime") {
//      customerSelectJoseAssertion(dob === OffsetDateTime.parse("1987-03-23T00:00:00Z"))
//    },
//    testM("Can select with property binary operator with ZonedLocalDate") {
//      customerSelectJoseAssertion(dob === ZonedDateTime.parse("1987-03-23T00:00:00Z"))
//    },
//    testM("Can select with property binary operator with Instant") {
//      customerSelectJoseAssertion(dob === Instant.parse("1987-03-23T00:00:00Z"))
//    },
//    testM("Can select with property binary operator with numbers") {
//      case class OrderDetails(orderId: UUID, product_id: UUID, quantity: Int, unitPrice: BigDecimal)
//
//      val orderDetailQuantity  = 3
//      val orderDetailUnitPrice = BigDecimal(80.0)
//      val condition            = (quantity === orderDetailQuantity) && (unitPrice === orderDetailUnitPrice)
//      val query                =
//        select(fkOrderId ++ fkProductId ++ quantity ++ unitPrice) from orderDetails where (condition)
//
//      println(renderRead(query))
//
//      val expected =
//        Seq(
//          OrderDetails(
//            UUID.fromString("763a7c39-833f-4ee8-9939-e80dfdbfc0fc"),
//            UUID.fromString("105a2701-ef93-4e25-81ab-8952cc7d9daa"),
//            orderDetailQuantity,
//            orderDetailUnitPrice
//          )
//        )
//
//      val testResult = execute(query)
//        .to[UUID, UUID, Int, BigDecimal, OrderDetails] { case row =>
//          OrderDetails(row._1, row._2, row._3, row._4)
//        }
//
//      val assertion = for {
//        r <- testResult.runCollect
//      } yield assert(r)(hasSameElementsDistinct(expected))
//
//      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    },
    testM("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = (select(customerId ++ fName ++ lName ++ dob) from customers).limit(1).offset(1).orderBy(fName)

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

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    /*
     * This is a failing test for aggregation function.
     * Uncomment it when aggregation function handling is fixed.
     */
    // testM("Can count rows") {
    //   val query = select { Count(userId) } from users

    //   val expected = 5L

    //   val result = new ExecuteBuilder(query).to[Long, Long](identity).provideCustomLayer(executorLayer)

    //   for {
    //     r <- result.runCollect
    //   } yield assert(r.head)(equalTo(expected))
    // },
    testM("Can select from joined tables (inner join)") {
      val query = select(fName ++ lName ++ orderDate) from (customers join orders).on(fkCustomerId === customerId)

      println(renderRead(query))

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(
        Row("Ronald", "Russell", LocalDate.parse("2019-03-25")),
        Row("Ronald", "Russell", LocalDate.parse("2018-06-04")),
        Row("Alana", "Murray", LocalDate.parse("2019-08-19")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
        Row("Ronald", "Russell", LocalDate.parse("2020-03-19")),
        Row("Alana", "Murray", LocalDate.parse("2020-05-11")),
        Row("Alana", "Murray", LocalDate.parse("2019-02-21")),
        Row("Ronald", "Russell", LocalDate.parse("2018-05-06")),
        Row("Mila", "Paterso", LocalDate.parse("2019-02-11")),
        Row("Terrence", "Noel", LocalDate.parse("2019-10-12")),
        Row("Ronald", "Russell", LocalDate.parse("2019-01-29")),
        Row("Terrence", "Noel", LocalDate.parse("2019-02-10")),
        Row("Ronald", "Russell", LocalDate.parse("2019-09-27")),
        Row("Alana", "Murray", LocalDate.parse("2018-11-13")),
        Row("Jose", "Wiggins", LocalDate.parse("2020-01-15")),
        Row("Terrence", "Noel", LocalDate.parse("2018-07-10")),
        Row("Mila", "Paterso", LocalDate.parse("2019-08-01")),
        Row("Alana", "Murray", LocalDate.parse("2019-12-08")),
        Row("Mila", "Paterso", LocalDate.parse("2019-11-04")),
        Row("Mila", "Paterso", LocalDate.parse("2018-10-14")),
        Row("Terrence", "Noel", LocalDate.parse("2020-04-05")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
        Row("Terrence", "Noel", LocalDate.parse("2019-05-14")),
        Row("Mila", "Paterso", LocalDate.parse("2020-04-30"))
      )

      val result = execute(
        query
          .to[String, String, LocalDate, Row] { case row =>
            Row(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select using like") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob) from customers where (fName like "Jo%")

      println(renderRead(query))
      val expected = Seq(
        Customer(
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
          "Jose",
          "Wiggins",
          LocalDate.parse("1987-03-23")
        )
      )

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Transactions is returning the last value") {
      val query = select(customerId) from customers

      val result = execute(
        ZTransaction(query) *> ZTransaction(query)
      ).use(ZIO.succeed(_))

      val assertion = assertM(result.flatMap(_.runCollect))(hasSize(Assertion.equalTo(5))).orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Transaction is failing") {
      val query = select(customerId) from customers

      val result = execute(
        ZTransaction(query) *> ZTransaction.fail(new Exception("failing")) *> ZTransaction(query)
      ).mapError(_.getMessage).use(ZIO.succeed(_))

      assertM(result.flip)(equalTo("failing")).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
