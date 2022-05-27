package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps

object OracleModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._

//  private def customerSelectJoseAssertion[F: Features.IsNotAggregated](
//    condition: Expr[F, customers.TableType, Boolean]
//  ) = {
//    case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)
//
//    val query =
//      select(customerId, fName, lName, verified, dob).from(customers).where(condition)
//
//    val expected =
//      Seq(
//        Customer(
//          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
//          "Jose",
//          "Wiggins",
//          false,
//          LocalDate.parse("1987-03-23")
//        )
//      )
//
//    val testResult = execute(query).map { case row =>
//      Customer(row._1, row._2, row._3, row._4, row._5)
//    }
//
//    val assertion = for {
//      r <- testResult.runCollect
//    } yield assert(r)(hasSameElementsDistinct(expected))
//
//    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//  }

  override def specLayered = suite("Postgres module")(
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers) where (verified isNotTrue)
      println(renderDelete(query))

      val expected = 1
      val result = execute(query)

      assertZIO(result)(equalTo(expected))
    },
    test("Can delete all from a single table") {
      val query = deleteFrom(customers)
      println(renderDelete(query))

      val expected = 5
      val result = execute(query)

      assertZIO(result)(equalTo(expected))
    },
  ) @@ sequential
}
