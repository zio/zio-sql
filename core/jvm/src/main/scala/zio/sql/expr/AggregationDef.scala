package zio.sql.expr

import zio.sql.typetag.TypeTag
import zio.sql.Features

final case class AggregationDef[-A, +B](name: FunctionName) { self =>

  def apply[F, Source, B1 >: B](expr: Expr[F, Source, A])(implicit
    typeTag: TypeTag[B1]
  ): Expr[Features.Aggregated[F], Source, B1] =
    Expr.AggregationCall[F, Source, A, B1](expr, self)
}

object AggregationDef {
  val Count                                      = AggregationDef[Any, Long](FunctionName("count"))
  val Sum                                        = AggregationDef[Double, Double](FunctionName("sum"))
  val SumInt                                     = AggregationDef[Int, Int](FunctionName("sum"))
  val SumDec                                     = AggregationDef[BigDecimal, BigDecimal](FunctionName("sum"))
  val Avg                                        = AggregationDef[Double, Double](FunctionName("avg"))
  val AvgDec                                     = AggregationDef[BigDecimal, BigDecimal](FunctionName("avg"))
  def Min[F, A, B: TypeTag](expr: Expr[F, A, B]) = AggregationDef[B, B](FunctionName("min"))(expr)
  def Max[F, A, B: TypeTag](expr: Expr[F, A, B]) = AggregationDef[B, B](FunctionName("max"))(expr)
}
