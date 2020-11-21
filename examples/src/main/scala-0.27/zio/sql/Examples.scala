package zio.sql

import zio.sql.sqlserver.SqlServerModule

object Examples extends App with ShopSchema with SqlServerModule {
  import this.AggregationDef._
  import this.FunctionDef._
  import this.OrderDetails._
  import this.Orders._
  import this.Users._

  /*
  val basicSelect = select(fName ++ lName) from users
  println(renderRead(basicSelect))

  //select first_name as first, last_name as last from users
  val basicSelectWithAliases = select {
    (fName as "first") ++ (lName as "last")
  } from users
  println(renderRead(basicSelectWithAliases))

  //select top 2 first_name, last_name from users order by last_name, first_name desc
  val selectWithRefinements =
    (select(fName ++ lName) from users)
      .orderBy(lName, fName.desc)
      .limit(2)
  println(renderRead(selectWithRefinements))

  case class Person(fname: String, lname: String)
  */
}