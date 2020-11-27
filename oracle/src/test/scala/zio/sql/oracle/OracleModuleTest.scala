package zio.sql.oracle

//import java.time.LocalDate
//
//import zio.Cause
//import zio.test._
//import zio.test.Assertion._

/* NOTE: Test is failing due to Oracle TestContainer timeZone issue
 * Ref: https://github.com/testcontainers/testcontainers-java/issues/2313
 * */

object OracleModuleTest { //extends OracleRunnableSpec with ShopSchema {

//  import this.Customers._
//  import this.Orders._

//  val spec = suite("Oracle module")(
//    testM("Can select from single table") {
//      case class Customer(id: String, fname: String, lname: String, dateOfBirth: LocalDate)
//
//      val query = select(customerId ++ fName ++ lName ++ dob) from customers
//
//      println(renderRead(query))
//      val expected =
//        Seq(
//          Customer(
//            "60b01fc9-c902-4468-8d49-3c0f989def37",
//            "Ronald",
//            "Russell",
//            LocalDate.parse("1983-01-05")
//          ),
//          Customer(
//            "f76c9ace-be07-4bf3-bd4c-4a9c62882e64",
//            "Terrence",
//            "Noel",
//            LocalDate.parse("1999-11-02")
//          ),
//          Customer(
//            "784426a5-b90a-4759-afbb-571b7a0ba35e",
//            "Mila",
//            "Paterso",
//            LocalDate.parse("1990-11-16")
//          ),
//          Customer(
//            "df8215a2-d5fd-4c6c-9984-801a1b3a2a0b",
//            "Alana",
//            "Murray",
//            LocalDate.parse("1995-11-12")
//          ),
//          Customer(
//            "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
//            "Jose",
//            "Wiggins",
//            LocalDate.parse("1987-03-23")
//          )
//        )
//
//      val testResult = execute(query)
//        .to[String, String, String, LocalDate, Customer] { case row =>
//          Customer(row._1, row._2, row._3, row._4)
//        }
//
//      val assertion = for {
//        r <- testResult.runCollect
//      } yield assert(r)(hasSameElementsDistinct(expected))
//
//      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    },
//    // NOTE: The below test case is failing because of extra true coming in the where clause of select query.
//    // Need to fix it in the coreJVM module, then this test case should pass
//
//    // testM("Can select with property operator") {
//    //   case class Customer(id: String, fname: String, lname: String, verified: Int, dateOfBirth: LocalDate)
//
//    //   val query =
//    //     select(
//    //       customerId ++ fName ++ lName ++ verified ++ dob
//    //     ) from customers where (verified === 0)
//
//    //   println(renderRead(query))
//
//    //   val expected =
//    //     Seq(
//    //       Customer(
//    //         "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
//    //         "Jose",
//    //         "Wiggins",
//    //         0,
//    //         LocalDate.parse("1987-03-23")
//    //       )
//    //     )
//
//    //   val testResult = execute(query)
//    //     .to[String, String, String, Int, LocalDate, Customer] { case row =>
//    //       Customer(row._1, row._2, row._3, row._4, row._5)
//    //     }
//
//    //   val assertion = for {
//    //     r <- testResult.runCollect
//    //   } yield assert(r)(hasSameElementsDistinct(expected))
//
//    //   assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    // },
//    // NOTE: Oracle 11g doesn't support Limit and Offset
//    testM("Can select from single table with rownum") {
//      case class Customer(id: String, fname: String, lname: String, dateOfBirth: LocalDate)
//
//      val query = (select(customerId ++ fName ++ lName ++ dob) from customers).limit(1)
//
//      println(renderRead(query))
//
//      val expected =
//        Seq(
//          Customer(
//            "60b01fc9-c902-4468-8d49-3c0f989def37",
//            "Ronald",
//            "Russell",
//            LocalDate.parse("1983-01-05")
//          )
//        )
//
//      val testResult = execute(query)
//        .to[String, String, String, LocalDate, Customer] { case row =>
//          Customer(row._1, row._2, row._3, row._4)
//        }
//
//      val assertion = for {
//        r <- testResult.runCollect
//      } yield assert(r)(hasSameElementsDistinct(expected))
//
//      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    },
//    /*
//     * This is a failing test for aggregation function.
//     * Uncomment it when aggregation function handling is fixed.
//     */
//    // testM("Can count rows") {
//    //   val query = select { Count(userId) } from users
//
//    //   val expected = 5L
//
//    //   val result = new ExecuteBuilder(query).to[Long, Long](identity).provideCustomLayer(executorLayer)
//
//    //   for {
//    //     r <- result.runCollect
//    //   } yield assert(r.head)(equalTo(expected))
//    // },
//    testM("Can select from joined tables (inner join)") {
//      val query = select(fName ++ lName ++ orderDate) from (customers join orders).on(fkCustomerId === customerId)
//
//      println(renderRead(query))
//
//      case class Row(firstName: String, lastName: String, orderDate: LocalDate)
//
//      val expected = Seq(
//        Row("Ronald", "Russell", LocalDate.parse("2019-03-25")),
//        Row("Ronald", "Russell", LocalDate.parse("2018-06-04")),
//        Row("Alana", "Murray", LocalDate.parse("2019-08-19")),
//        Row("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
//        Row("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
//        Row("Ronald", "Russell", LocalDate.parse("2020-03-19")),
//        Row("Alana", "Murray", LocalDate.parse("2020-05-11")),
//        Row("Alana", "Murray", LocalDate.parse("2019-02-21")),
//        Row("Ronald", "Russell", LocalDate.parse("2018-05-06")),
//        Row("Mila", "Paterso", LocalDate.parse("2019-02-11")),
//        Row("Terrence", "Noel", LocalDate.parse("2019-10-12")),
//        Row("Ronald", "Russell", LocalDate.parse("2019-01-29")),
//        Row("Terrence", "Noel", LocalDate.parse("2019-02-10")),
//        Row("Ronald", "Russell", LocalDate.parse("2019-09-27")),
//        Row("Alana", "Murray", LocalDate.parse("2018-11-13")),
//        Row("Jose", "Wiggins", LocalDate.parse("2020-01-15")),
//        Row("Terrence", "Noel", LocalDate.parse("2018-07-10")),
//        Row("Mila", "Paterso", LocalDate.parse("2019-08-01")),
//        Row("Alana", "Murray", LocalDate.parse("2019-12-08")),
//        Row("Mila", "Paterso", LocalDate.parse("2019-11-04")),
//        Row("Mila", "Paterso", LocalDate.parse("2018-10-14")),
//        Row("Terrence", "Noel", LocalDate.parse("2020-04-05")),
//        Row("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
//        Row("Terrence", "Noel", LocalDate.parse("2019-05-14")),
//        Row("Mila", "Paterso", LocalDate.parse("2020-04-30"))
//      )
//
//      val result = execute(query)
//        .to[String, String, LocalDate, Row] { case row =>
//          Row(row._1, row._2, row._3)
//        }
//
//      val assertion = for {
//        r <- result.runCollect
//      } yield assert(r)(hasSameElementsDistinct(expected))
//
//      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    }
//  )

}
