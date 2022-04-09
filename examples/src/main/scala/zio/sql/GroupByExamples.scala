package zio.sql

import zio.sql.sqlserver.SqlServerModule

object GroupByExamples extends App with ShopSchema with SqlServerModule {
  import AggregationDef._
  import ColumnSet._

  val productTable = (
    string("id") ++
      string("name") ++
      int("amount") ++
      double("price")
  ).table("product")

  val (id, name, amount, price) = productTable.columns

  select(Count(price))
    .from(productTable)
    .groupBy(price)

  val e = Sum(price) > 10

  def testF[F, A, B](value: Expr[F, A, B])(implicit in: Features.IsFullyAggregated[F]) = ???

  def test2[F, A, B](value: Expr[F, A, B])(implicit i: Features.IsPartiallyAggregated[F]): i.Unaggregated = ???

  val orderValue = select(name ++ Sum(price))
    .from(productTable)
    .groupBy(name, price)
    .having(Sum(price) > 10)

  select(Sum(price))
    .from(productTable)
    .groupBy(name)
    .having(Sum(price) > 10)

  select(name ++ amount ++ price)
    .from(productTable)
    .groupBy(name, amount, price)
    .having(Sum(price) > 10)

  select(amount)
    .from(productTable)
    .groupBy(amount)
    .having(amount > 10)

  select(Sum(price))
    .from(productTable)
    .groupBy(name)
    .having(name > 10)

  select(price)
    .from(productTable)
    .groupBy(price)
    .having(Count(price) > 10)

  // Following should not compile
  // select(amount ++ price)
  //   .from(productTable)
  //   .groupBy(amount)
  //   .having(amount > 10)

  // select(price)
  //   .from(productTable)
  //   .groupBy(name)
  //   .having(name > 10)

  // select(price ++ name)
  //   .from(productTable)
  //   .groupBy(price)
  //   .having(Count(price) > 10)

  // select(price)
  //   .from(productTable)
  //   .having(Count(price) > 10)
}
