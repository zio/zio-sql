package zio.sql

import zio.test.Assertion.anything
import zio.test.assert
import zio.schema.Schema
import zio.test.ZIOSpecDefault

object GroupByHavingSpec extends ZIOSpecDefault {

  import AggregatedProductSchema._

  def spec = suite("groupBy")(
    test("works") {
      assert(orderValue)(anything)
    }
  )
}

object AggregatedProductSchema {
  val sqldsl = new Sql { self =>
    override def renderDelete(delete: self.Delete[_]): String = ???
    override def renderRead(read: self.Read[_]): String       = ???
    override def renderUpdate(update: self.Update[_]): String = ???

    override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???
  }
  import sqldsl.ColumnSet._
  import sqldsl.AggregationDef._
  import sqldsl._

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

  val q  = test2(amount > 10)
  val q1 = test2(Count(amount) > 10)

  val qw = amount > 10

  testF(e)
  testF(Expr.literal(true))

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
