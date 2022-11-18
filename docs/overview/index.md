---
id: overview_index
title: "Quick introduction"
---

TODO: Some initial statement about ZIO SQL

## Installation

ZIO SQL is packaged into separate modules for different databases. Depending on which of these (currently supported) systems you're using, you will need to add one of the following dependencies:
```scala
//PostgreSQL
libraryDependencies += "dev.zio" %% "zio-sql-postgres" % zioSqlVersion

//MySQL
libraryDependencies += "dev.zio" %% "zio-sql-mysql" % zioSqlVersion

//Oracle
libraryDependencies += "dev.zio" %% "zio-sql-oracle" % zioSqlVersion

//SQL Server
libraryDependencies += "dev.zio" %% "zio-sql-sqlserver" % zioSqlVersion
```

## Imports and modules

Most of the needed imports will be resolved with
```scala
import zio.sql._
```

ZIO SQL relies heavily on path dependent types, so to use most of the features you need to be in the scope of one of the database modules:

```scala
trait MyRepositoryModule extends PostgresModule {

  // your ZIO SQL code here

}

// other available modules are MysqlModule, OracleModule and SqlServerModule
```

We will assume this scope in the following examples.

## Table schema

In order to construct correct and type-safe queries, we need to describe tables by writing user defined data type - case class in which
name of the case class represents table name, field names represent column names and field types represent column types.

Values that will represent tables in DSL are then created by calling `defineTable` method which takes case class type parameter.
In order for `defineTable` to work, user need to provide implicit `Schema` of data type.

```scala
import java.util.UUID
import zio.sql.postgresql.PostgresJdbcModule
import java.time._

object Repository extends PostgresJdbcModule {
  final case class Product(id: UUID, name: String, price: BigDecimal)
  implicit val productSchema = DeriveSchema.gen[Product]

  val products = defineTableSmart[Product]
  
  final case class Order(id: UUID, productId: UUID, quantity: Int, orderDate: LocalDate)
  implicit val orderSchema = DeriveSchema.gen[Order]
  
  val orders = defineTable[Order]
}
```

`defineTable` method is overloaded with an alternative that takes table name as an input. User can also specify table name using `@name` annotation.
Alternatively user can use `defineTableSmart` method which will smartly pluralize table name according to english grammar.
`OrderOrigin` -> `order_origins`
`Foot` -> `feet`
`PersonAddress` -> `person_addresses`
Field names are also converted to lowercase and snake case.
`productId` -> `product_id` and so on.


## Table schema decomposition

Once we have our table definition we need to decompose table into columns which we will use in queries.
Using the previous example with `Product` and `Order` table
```scala
val (id, name, price) = products.columns

val (orderId, productId, quantity) = orders.columns
```

## Selects

Simple select.

```scala
val allProducts = select(productId, name, price).from(products)
```

Using `where` clause.

```scala
def productById(id: UUID) = 
  select(productId, name, price).from(products).where(productId === id)
```

Inner join.

```scala
val ordersWithProductNames = 
  select(orderId, name).from(products.join(orders).on(productId === fkProductId))
```

Left outer join.

```scala
val leftOuter = 
  select(orderId, name).from(products.leftOuter(orders).on(productId === fkProductId))
```

Right outer join.

```scala
val rightOuter = 
  select(orderId, name).from(products.rightOuter(orders).on(productId === fkProductId))
```

Using `limit` and `offset`

```scala
val limitedResults = 
  select(orderId, name)
    .from(products.join(orders)
    .on(productId === fkProductId))
    .limit(5)
    .offset(10)
```

## Inserts

```scala
insertInto(products)
    (productId, name, price)
  .values((UUID.randomUUID(), "Zionomicon", 10.5))
```

## Updates

TODO: details

## Deletes

TODO: details

## Transactions

TODO: details

## Printing queries

TODO: details

## Running queries

TODO: details
