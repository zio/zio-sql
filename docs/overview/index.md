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

Table schemas are required to later construct correct and type-safe queries. Table schemas are created from `ColumnSet`s that are composed out of single columns, by calling a method `table` (on the `ColumnSet`).

```scala
import ColumnSet._

val products =
  (uuid("id") ++ string("name") ++ bigDecimal("price")).table("products")

val orders = 
  (uuid("id") ++ uuid("product_id") ++ int("quantity") ++ localDate("order_date")).table("orders")
```

Column sets are composable - this could be useful when your tables share some common columns.

```scala
import ColumnSet._

//common columns
val auditColumns = zonedDateTime("created_at") ++ zonedDateTime("updated_at")

//products definition
val productColumns = uuid("id") ++ string("name") ++ bigDecimal("price") ++ auditColumns

val products = productColumns.table("products")

//orders definition
val orderColumns = uuid("id") ++ uuid("product_id") ++ int("quantity") ++ localDate("order_date") ++ auditColumns

val orders = orderColumns.table("orders")
```

## Table schema decomposition

TODO

## Selects

Simple select.

```scala
val allProducts = select(productId ++ name ++ price).from(products)
```

Using `where` clause.

```scala
def productById(id: UUID) = 
  select(productId ++ name ++ price).from(products).where(productId === id)
```

Inner join.

```scala
val ordersWithProductNames = 
  select(orderId ++ name).from(products.join(orders).on(productId === fkProductId))
```

Left outer join.

```scala
val leftOuter = 
  select(orderId ++ name).from(products.leftOuter(orders).on(productId === fkProductId))
```

Right outer join.

```scala
val rightOuter = 
  select(orderId ++ name).from(products.rightOuter(orders).on(productId === fkProductId))
```

Using `limit` and `offset`

```scala
val limitedResults = 
  select(orderId ++ name)
    .from(products.join(orders)
    .on(productId === fkProductId))
    .limit(5)
    .offset(10)
```

## Inserts

```scala
insertInto(products)
    (productId ++ name ++ price)
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
