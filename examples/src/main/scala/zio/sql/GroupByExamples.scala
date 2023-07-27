package zio.sql

import zio.schema.DeriveSchema
import zio.sql.postgresql.PostgresJdbcModule
import zio.sql.expr.AggregationDef._
import zio.sql.table._

object GroupByExamples extends App with PostgresJdbcModule {

  case class Product(id: Int, name: String, amount: Int, price: Double)

  implicit val productSchema = DeriveSchema.gen[Product]

  val productTable = Table.defineTable[Product]

  val (id, name, amount, price) = productTable.columns

  select(Count(price))
    .from(productTable)
    .groupBy(price)

  val e = Sum(price) > 10

  val orderValue = select(name, Sum(price))
    .from(productTable)
    .groupBy(name, price)
    .having(Sum(price) > 10)

  execute(orderValue)

  select(Sum(price))
    .from(productTable)
    .groupBy(name)
    .having(Sum(price) > 10)

  select(name, amount, price)
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
    .having(name > "z")

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
  //   .having(name > "")

  // select(price ++ name)
  //   .from(productTable)
  //   .groupBy(price)
  //   .having(Count(price) > 10)

  // execute(select(name, Sum(price)).from(productTable))

  select(price)
    .from(productTable)
    .groupBy(price)
    .having(Count(price) > 10)

  select(Sum(price))
    .from(productTable)
    .having(Sum(price) > 10)

  select(price)
    .from(productTable)
    .groupBy(price, amount)
    .having(amount > 200)

  select(amount)
    .from(productTable)
    .groupBy(amount)
    .having(Sum(price) > 200)

  // select(price)
  //   .from(productTable)
  //   .groupBy(price)
  //   .having(amount > 200)

  // select(amount)
  //   .from(productTable)
  //   .having(Sum(price) > 200)

  // select(amount)
  //  .from(productTable)
  //  .groupBy(amount)
  //  .having(amount > 10)
  //  .where(amount > 10)

  select(amount)
    .from(productTable)
    .groupBy(amount)
    .having(amount > 10)
}
