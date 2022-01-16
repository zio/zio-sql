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

You can compose column set out of smaller column sets. This could be useful when your tables share some common set of columns.

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

## Selects

TODO: details

## Inserts

In this chapter we will explore how to write type safe inserts with zio-sql. 

### Table description
As usual, in order to use the DSL, first thing we need to do is to create meta-model of our table. Let’s imagine we have a *customers* table in postgres (in case of a different database just extend the appropriate module)
```scala
import zio.sql.postgresql.PostgresModule

trait TableModel extends PostgresModule {

  import ColumnSet._

  val customers =
      (uuid("id") ++ localDate(“date_of_birth”) ++ string("first_name") ++ string("last_name") ++ boolean("verified_customer") ++ zonedDateTime("created"))
        .table("customers")

  val customerId :*: dob :*: fName :*: lName :*: verified :*: created :*: _ = customers.columns
} 
```
Then, to use zio-sql ’s inserts, just mix in `TableModel` trait from above, to your repository. 

In case you’re wondering what those extracted columns are (customerId, dob etc), they are of a type called *Expr*.
`Expr[F, A, B]` is fundamental abstraction in zio-sql which basically represents description of any SQL expression of type `B`, having a source of type `A` and a phantom type `F`. 
To give specific example, type of `fName` is 
```scala
Expr[Features.Source[String(“first_name”)], customers.TableType, String]. 
```
This gives DSL huge power to remember table from which the column comes from, type of the columns and what kind of Expr we are dealing with. Don’t worry, you don’t need to remember any of this, but from now on we will use those Expr instances in our inserts.

In general, DSL is giving us two options how to approach inserts. We can insert either tuple values or used defined case class - which requires zio-schema instance (more on that later). 
Also your custom data type or tuple need to consist only of the types for which there is a `TypeTag` instance defined. Each sql module has a finite set of such types - those are the types that particular module can work with. In other words, types inside your tuples or case class need to correspond with the types of the extracted Exprs.

### Insert tuples
Let’s say we want to build the query like the following one:
```sql
insert into 
  customers(id, date_of_birth, first_name, last_name, verified_customer, created)
values
  ('60b01fc9-c902-4468-8d49-3c0f989def37', ‘1983-01-05’, 'Ronald', 'Russell', true, '2020-11-21 19:10:25+00')
```
zio-sql gives us nice typesafe DSL that feels similar to writing SQL:
```scala
insertInto(customers)
    (customerId ++ dob ++ fName ++ lName ++ verified ++ created)
  .values((UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Ronald", "Russell", true, ZonedDateTime.now()))
```
Compiler verifies your inserts and your query fails with compile-time error at any of the following situations:
- you mess up the order of values - e.g. you put Boolean where String is expected
- you don’t specify all the not null columns of the table
- you try to insert to columns from another table

Some  details about syntax: `insertInto` method takes two value parameters. One is our table `customers` that we created before in *Table description* section. The other is an *HList like* collection of Expr’s, called `Selection`. You create it by appending Exprs with “++” operator. 
`values` method takes a Tuple6 of type (UUID, LocalDate, String, String, Boolean, ZonedDateTime). The required tuple is dependent on combination of Exprs. Just like with normal sql insert, you could swap `fName` with `dob` Expr and corresponding values and your query will work just fine. Compiler will only let you build such queries that won’t explode in runtime (in case you described your table correctly of course ! )

If we need to insert multiple values at once, all we need to do is to create any `Seq` of tuples and stick it into the overloaded `values` method. 
```scala
val data =
        List(
          (UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Ronald1", "Russel1", true,  ZonedDateTime.now()),
          (UUID.randomUUID(), LocalDate.ofYearDay(1980, 1), "Ronald2", "Russel2", false,  ZonedDateTime.now()),
          (UUID.randomUUID(), LocalDate.ofYearDay(1970, 1), "Ronald3", "Russel3", true,  ZonedDateTime.now())
        )

val query = insertInto(customers)(
        customerId ++ dob ++ fName ++ lName ++ verified ++ createdString ++ createdTimestamp
      ).values(data)
```

In this case, data is of type `List[(UUID, LocalDate, String, String, Boolean, ZonedDateTime)]`

### Insert custom case class
ZIO SQL lets you insert also your own case classes.
Let’s define a *customer* case class:

```scala
final case class Customer(
        id: UUID,
        dateOfBirth: LocalDate,
        firstName: String,
        lastName: String,
        verified: Boolean,
        createdTimestamp: ZonedDateTime
      )
```

In this case, the name of the fields makes no difference. Similarly to writing sql, the order of the fields is important.

zio-sql also needs an implicit instance of zio-schema for your data type. You can define it easily:

```scala
import zio.schema.DeriveSchema
implicit val customerSchema = DeriveSchema.gen[Customer]
```

Then your insert looks almost the same as before:
```scala
val data: Customer = Customer(UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Ronald", "Russel", true, ZonedDateTime.now())

val query = insertInto(customers)(
        customerId ++ dob ++ fName ++ lName ++ verified ++ createdString ++ createdTimestamp
      ).values(data)
```
Or you can insert multiple rows at once. Just define data as a `List` or any collection of your choice.

```scala
val data : List[Customer] = ???
```

### Show generated SQL query 

In case you want to see the exact query that zio-sql generated, you can use `renderInsert` method inside repo that has PostgresModule (or TableModel from above example) mixed in.
```scala
val query = insertInto(customers)(
        customerId ++ dob ++ fName ++ lName ++ verified ++ createdString ++ createdTimestamp
      ).values((UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Ronald", "Russell", true, ZonedDateTime.now()))

val sqlString: String = renderInsert(query)
```

### Execute the query

In order to execute a query, we use `execute` method inside repo that has PostgresModule (or TableModel from the above example) mixed in.
```scala
val query = insertInto(customers)(
        customerId ++ dob ++ fName ++ lName ++ verified ++ createdString ++ createdTimestamp
      ).values((UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Ronald", "Russell", true, ZonedDateTime.now()))

val executed : ZIO[Has[SqlDriver], Exception, Int] = execute(query)
```
As the type of `executed` indicates, you need to provide an `SqlDriver` in order to run this effect. The result *Int* is the number of rows updated.

### More examples
More examples can be found in zio-sql test suite (`PostgresModuleSpec`, `SqlServerModuleSpec`, …) or in zio-sql-example application in resources.

### What is missing
As of now - Q1 2022 - zio-sql contributors is actively working on:
- returning generated IDs from inserts
- introduce nullable columns - for which user won’t need to input values
- introduce auto generated columns - for which user cannot input values

## Subqueries & Correlated subqueries

The goal of ZIO SQL is to give users the ability to describe also queries much more complex than just simple selects or joins. In this section we will introduce a few examples of subqueries and correlated subqueries. In case you will find a query which is not possible to write with zio-sql - or the generated sql query looks differently then expected - please contact us on discord and we will try to add your use case to the next release :) Now let’s explore what is possible today.

### Subquery
Subquery is a query which is a part of another query. It’s executed first - before outer query - and then its result is used in outer query.

Now let’s say we want to build following query (this is on MSSQL Server) :

```sql
select order_id, product_id, unit_price 
from order_details
where unit_price > (select AVG(price) from product_prices )
```
We want to match details about orders, but we are interested only in those orders where price is higher than average price of all the products from `product_prices` table. 

This is the meta model that we are working with:
```scala
val productPrices =
      (uuid("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")

val id :*: effective :*: price :*: _ = productPrices.columns

val orderDetails =
      (uuid("order_id") ++ uuid("product_id") ++ bigDecimal("unit_price")).table("order_details")

val orderDetailsId :*: productId :*: unitPrice :*: _ = orderDetails.columns
```
We can create query very easily. In fact, just type it like a regular sql query and let your IDE auto completion guide you! 
```scala
val query = select(orderDetailsId ++ productId ++ unitPrice)
        .from(orderDetails)
        .where(
          unitPrice > select(Avg(price)).from(productPrices)
        )
```
Then you can either execute the query to selected types or inspect sql query represented as a String.
You just need a custom data type (Row in our example) to encapsulate results of a selection.
```scala
case class Row(orderId: UUID, productId: UUID, unitPrice: BigDecimal)

val result: ZStream[Has[SqlDriver],Exception,Row] = execute(query.to[UUID, UUID, BigDecimal, Row](Row.apply))

val sqlQuery: String = renderRead(query)
```
Similarly you can use subqueries inside `select` clause.

### Correlated subqueries
Correlated subqueries are the ones that are executed after the outer query. They can be dependent on the result of the outer query and therefore they are executed for each resulting row of the outer query.

Lets say we want to build the following query:
```sql
select first_name, last_name, 
		( select count(orders.id) from orders where customers.id = orders.customer_id ) as "count" 
from customers
``` 
This would return the count of orders for each customer.

Description of tables:
```scala
val customers = (uuid("id") ++ string("first_name") ++ string("last_name"))).table("customers")

val customerId :*: fName :*: lName _ = customers.columns

val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns
```
ZIO SQL query:
```scala
val subquery =
        customers.subselect(Count(orderId)).from(orders).where(fkCustomerId === customerId)

val query = select(fName ++ lName ++ (subquery as "Count")).from(customers)
```
All of these examples and more can be found and run in zio-sql tests.

### Correlated subquery in from clause & Derived tables
Just one last, a little more complex example before we wrap up this section, for which we would use the same *customers* and *orders* tables as before.
```scala
val customers = (uuid("id") ++ string("first_name") ++ string("last_name"))).table("customers")

val customerId :*: fName :*: lName _ = customers.columns

val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns
```
Imagine we want to write a query that selects all customers with the date of their last order. If you approach this problem with JOIN, you end up with one row of a customer with the newest order. In fact, this is a good example of correlated subquery inside `from` clause, where subquery needs to access `customer_id` of the outer query. For this type of problems postgres introduced **LATERAL** keyword and MSSQL Server have **CROSS APPLY** and **OUTER APPLY**.
```sql
select customers.id, customers.first_name, customers.last_name, derived.order_date
                from customers,
                lateral  (
                     select orders.order_date
                     from orders
                     where customers.id = orders.customer_id
                     order by orders.order_date desc limit 1 ) derived order by derived.order_date desc
```
Now it’s starting to be a little more complicated. First we need to create a `subselect` which can access columns from another source table - `customers` in our case. Then we specify this source as a type parameter to `subselect`. In order to build the whole query we also need `derived.order_date` which is coming from `derived` table, so that we can extract that column. We create `derivedTable` by calling `asTable(tableName: String)` method on `subselect`. 
```scala
 val derivedTable  = subselect[customers.TableType](orderDate)
        .from(orders)
        .limit(1)
        .where(customerId === fkCustomerId)
        .orderBy(Ordering.Desc(orderDate))
        .asTable("derived")

val orderDateDerived :*: _ = derivedTable
```
Finally, we have all the ingredients we need to describe our query with zio-sql.
```scala
import PostgresSpecific.PostgresSpecificTable._

val query =
        select(customerId ++ fName ++ lName ++ orderDateDerived)
          .from(customers.lateral(derivedTable))
          .orderBy(Ordering.Desc(orderDateDerived))
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
