package zio.sql.postgresql

import zio.test.TestAspect._
import zio.test._
import zio.sql.expr.AggregationDef._
import java.math.{ BigDecimal, RoundingMode }
import java.util.UUID

object AgregationSpec extends PostgresRunnableSpec with DbSchema {

  import OrderDetailsSchema._

  override def specLayered =
    suite("Postgres module with aggregate function SumInt, SumDec and avgDec ")(
      test("Can aggregate  colums   SumInt(Int column)  and SumDec(BigDdecimal colum)") {
        
        val query = select((SumDec(unitPrice) as "totalAmount"), (SumInt(quantity) as "soldQuantity"))
          .from(orderDetails)
          .where(orderDetailsProductId === UUID.fromString("7368ABF4-AED2-421F-B426-1725DE756895"))

        val result = execute(query).runCollect.map(_.toList).head
        for {
          r <- result
        } yield assertTrue(r._1 == BigDecimal.valueOf(215.99)) && assertTrue(r._2 == 40)
      },
      test("Can aggregate  colums  of typ  money () AvgDec(BigDdecimal colum)") {
        val query = select((AvgDec(unitPrice) as "AverageAmount"))
          .from(orderDetails)
          .where(orderDetailsProductId === UUID.fromString("7368ABF4-AED2-421F-B426-1725DE756895"))

        val result = execute(query).runCollect.map(_.toList).head
        for {
          r <- result
        } yield assertTrue(r.setScale(4, RoundingMode.CEILING) == BigDecimal.valueOf(10.7995))
      }
    ) @@ sequential

}
