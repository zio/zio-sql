---
id: index
title: "Introduction to ZIO SQL"
sidebar_label: "ZIO SQL"
---

ZIO SQL lets you write type-safe, type-inferred, and composable SQL queries in ordinary Scala, helping you prevent persistence bugs before they happen, and leverage your IDE to make writing SQL productive, safe, and fun.

@PROJECT_BADGES@

## Introduction

* **Type-safety**. ZIO SQL queries are type-safe by construction. Most classes of bugs can be detected at compile-time, shortening your feedback loop and helping you use your IDE to write correct queries.
* **Composable**. All ZIO SQL components are ordinary values, which can be transformed and composed in sensible ways. This uniformity and regularity means you have a lot of power in a small package.
* **Type-inferred**. ZIO SQL uses maximal variance and lower-kinded types, which means it features very good type inference. You can let Scala figure out the types required for type-safe SQL.
* **No magic**. ZIO SQL does not need any macros or plug-ins to operate (everything is a value!), and it works across both Scala 2.x and Scala 3. Optionally, Scala schema can be created from database schemas.

ZIO SQL can be used as a library for modeling SQL in a type-safe ADT. In addition, ZIO SQL has a JDBC interface, which utilizes the type-safe SQL ADT for interacting with common JDBC databases.

For the JDBC module:

- Like Slick, ZIO SQL has an emphasis on type-safe SQL construction using Scala values and methods. However, ZIO SQL utilizes reified lenses, contravariant intersection types, and in-query nullability to improve ergonomics for end-users. Unlike Slick, the intention is to use names resembling SQL instead of trying to mimic the Scala collections.
- Like Doobie, ZIO SQL is purely functional, but ZIO SQL does compile-time query validation that catches most issues, and has rich ZIO integration, offering improved type-safety compared to monofunctor effects and minimal dependencies (depending only on ZIO).

ZIO SQL does not offer Language Integrated Queries (LINQ) or similar functionality. It is intended only as a data model for representing SQL queries and an accompanying lightweight JDBC-based executor.

## Current status: Non-production release

### Progress report towards 0.1

:heavy_check_mark: - good to go

:white_check_mark: - some more work needed

#### General features:

| Feature          | Progress           |
|:-----------------|:-------------------|
| Type-safe schema | :heavy_check_mark: |
| Type-safe DSL    | :heavy_check_mark: |
| Running Reads    | :heavy_check_mark: |
| Running Deletes  | :heavy_check_mark: |
| Running Updates  | :heavy_check_mark: |
| Running Inserts  | :heavy_check_mark: |
| Transactions     | :white_check_mark: |
| Connection pool  | :white_check_mark: |

#### Db-specific features:

| Feature       | PostgreSQL         | SQL Server         | Oracle             | MySQL              |
|:--------------|:-------------------|:-------------------|:-------------------|:-------------------|
| Render Read   | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Render Delete | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Render Update | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Render Insert | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Functions     | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Types         | :white_check_mark: |                    |                    | :white_check_mark: |
| Operators     |                    |                    |                    |                    |

## Installation

ZIO SQL is packaged into separate modules for different databases. Depending on which of these (currently supported) systems you're using, you will need to add one of the following dependencies:

```scala
//PostgreSQL
libraryDependencies += "dev.zio" %% "zio-sql-postgres" % "@VERSION@" 

//MySQL
libraryDependencies += "dev.zio" %% "zio-sql-mysql" % "@VERSION@"

//Oracle
libraryDependencies += "dev.zio" %% "zio-sql-oracle" % "@VERSION@"

//SQL Server
libraryDependencies += "dev.zio" %% "zio-sql-sqlserver" % "@VERSION@"
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
