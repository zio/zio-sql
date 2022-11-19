package zio.sql.postgresql

import zio.Cause
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object AgregationSpec extends PostgresRunnableSpec with DbSchema {

  import AggregationDef._
  import OrderDetails._

  override def specLayered =
    suite("Postgres module with aggregate function SumInt, SumDec and avgDec ")(
      test("Can aggregate  colums   SumInt(Int column)  and SumDec(BigDdecimal colum)") {

        val query = select((SumDec(unitPrice) as "totalAmount"), (SumInt(quantity) as "soldQuantity"))
          .from(orderDetails)
          .where(orderDetailsProductId === "7368ABF4-AED2-421F-B426-1725DE756895")

        val result    = execute(query).runCollect.map(_.toList).head
        val assertion = for {
          r <- result
        } yield assert(r._1)(equalTo(BigDecimal(215.99))) && assert(r._2)(equalTo(40))
        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      },
      test("Can aggregate  colums  of typ  money () AvgDec(BigDdecimal colum)") {
        val query = select((AvgDec(unitPrice) as "AverageAmount"))
          .from(orderDetails)
          .where(orderDetailsProductId === "7368ABF4-AED2-421F-B426-1725DE756895")

        val result    = execute(query).runCollect.map(_.toList).head
        val assertion = for {
          r <- result
        } yield assert(r)(equalTo(BigDecimal(10.7995)))
        assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
      }
    ) @@ sequential

}
