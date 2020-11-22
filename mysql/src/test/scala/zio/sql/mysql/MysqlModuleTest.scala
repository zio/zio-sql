package zio.sql.mysql

import java.time.LocalDate

import zio.Cause
import zio.test._
import zio.test.Assertion._
import scala.language.postfixOps

// NOTE: BUG: TestContainer is inserting the mock value with different dataTimeZone compare to Java API LocalDate
// The dates which are inserted are one day behind the actual date. For now values are tested with subtracting one day.
// This is the none issue with JDBC and mysql.
//For ref: https://stackoverflow.com/questions/54666536/date-one-day-backwards-after-select-from-mysql-db

object MysqlModuleTest extends MysqlRunnableSpec with ShopSchema {

  import this.Customers._
  import this.Orders._

  val spec = suite("Mysql module")(
    testM("Can select from single table") {
      case class Customer(id: String, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob) from customers

      println(renderRead(query))
      val expected =
        Seq(
          Customer(
            "60b01fc9-c902-4468-8d49-3c0f989def37",
            "Ronald",
            "Russell",
            LocalDate.parse("1983-01-04")
          ),
          Customer(
            "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-22")
          ),
          Customer(
            "784426a5-b90a-4759-afbb-571b7a0ba35e",
            "Mila",
            "Paterso",
            LocalDate.parse("1990-11-15")
          ),
          Customer(
            "df8215a2-d5fd-4c6c-9984-801a1b3a2a0b",
            "Alana",
            "Murray",
            LocalDate.parse("1995-11-11")
          ),
          Customer(
            "f76c9ace-be07-4bf3-bd4c-4a9c62882e64",
            "Terrence",
            "Noel",
            LocalDate.parse("1999-11-01")
          )
        )

      val testResult = execute(query)
        .to[String, String, String, LocalDate, Customer] { case row =>
          Customer(row._1, row._2, row._3, row._4)
        }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select with property operator") {
      case class Customer(id: String, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ verified ++ dob) from customers where (verified isNotTrue)

      println(renderRead(query))

      val expected =
        Seq(
          Customer(
            "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
            "Jose",
            "Wiggins",
            false,
            LocalDate.parse("1987-03-22")
          )
        )

      val testResult = execute(query)
        .to[String, String, String, Boolean, LocalDate, Customer] { case row =>
          Customer(row._1, row._2, row._3, row._4, row._5)
        }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select from single table with limit, offset and order by") {
      case class Customer(id: String, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = (select(customerId ++ fName ++ lName ++ dob) from customers).limit(1).offset(1).orderBy(fName)

      println(renderRead(query))

      val expected =
        Seq(
          Customer(
            "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-22")
          )
        )

      val testResult = execute(query)
        .to[String, String, String, LocalDate, Customer] { case row =>
          Customer(row._1, row._2, row._3, row._4)
        }

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
      val query = select(fName ++ lName ++ orderDate) from (customers join orders).on(
        fkCustomerId === customerId
      ) where (verified isNotTrue)

      println(renderRead(query))

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(
        Row("Jose", "Wiggins", LocalDate.parse("2019-08-29")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-01-22")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-03-06")),
        Row("Jose", "Wiggins", LocalDate.parse("2020-01-14"))
      )

      val result = execute(query)
        .to[String, String, LocalDate, Row] { case row =>
          Row(row._1, row._2, row._3)
        }

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )

}
