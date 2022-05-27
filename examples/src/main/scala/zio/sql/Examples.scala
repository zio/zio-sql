package zio.sql

import zio.sql.postgresql.PostgresJdbcModule

object Examples extends App with ShopSchema with PostgresJdbcModule {
  import this.AggregationDef._
  import this.FunctionDef._
  import this.OrderDetails._
  import this.Orders._
  import this.Users._

  // SELECT "users"."first_name", "users"."last_name" FROM "users"
  val basicSelect =
    select(fName, lName).from(users)
  println(renderRead(basicSelect))

  // SELECT "users"."age" + 2, concat_ws("users"."first_name",' ',"users"."last_name"), abs(-42.0) FROM "users" ORDER BY "users"."age" DESC LIMIT 10 OFFSET 20
  val selectWithFunctions =
    select(age + 2, ConcatWs3(fName, " ", lName), Abs(-42.0))
      .from(users)
      .limit(10)
      .offset(20)
      .orderBy(age.descending)
  println(renderRead(selectWithFunctions))

  // SELECT "users"."first_name", "users"."last_name" FROM "users" ORDER BY "users"."last_name", "users"."first_name" DESC LIMIT 2
  val selectWithRefinements =
    select(fName, lName)
      .from(users)
      .orderBy(lName, fName.desc)
      .limit(2)
  println(renderRead(selectWithRefinements))

  case class Person(fname: String, lname: String)

  // execute(selectWithRefinements).to(Person)
  // execute(selectWithRefinements).to((_, _))

  // DELETE FROM "users" WHERE "users"."first_name" = 'Terrence'
  val basicDelete =
    deleteFrom(users).where(fName === "Terrence")
  println(renderDelete(basicDelete))

  /*
    val deleteFromWithSubquery = deleteFrom(orders).where(fkUserId in {
      select(userId as "id") from users where (fName === "Fred") //todo fix issue #36
    }) */

  // SELECT "users"."first_name", "users"."last_name", "orders"."order_date" FROM "users" LEFT JOIN "orders" ON "orders"."usr_id" = "users"."id"
  val basicJoin =
    select(fName, lName, orderDate).from(users.leftOuter(orders).on(fkUserId === userId))
  println(renderRead(basicJoin))

  // UPDATE "users" SET "first_name" = 'foo', "last_name" = 'bar', "age" = "users"."age" + 1 WHERE true and "users"."age" > 100
  val basicUpdate =
    update(users)
      .set(fName, "foo")
      .set(lName, "bar")
      .set(age, age + 1)
      .where(age > 100)
  println(renderUpdate(basicUpdate))

  /*
    SELECT "users"."id", "users"."first_name", "users"."last_name", sum("order_details"."quantity" * "order_details"."unit_price"), sum(abs("order_details"."quantity"))
    FROM "users"
    INNER JOIN "orders" ON "users"."id" = "orders"."usr_id"
    LEFT JOIN "order_details" ON "orders"."id" = "order_details"."order_id"
    GROUP BY "users"."id", "users"."first_name", "users"."last_name" */
  val orderValues =
    select(
      userId,
      fName,
      lName,
      Sum(quantity * unitPrice),
      Sum(Abs(quantity))
    )
      .from(
        users
          .join(orders)
          .on(userId === fkUserId)
          .leftOuter(orderDetails)
          .on(orderId === fkOrderId)
      )
      .groupBy(userId, fName, lName)
  println(renderRead(orderValues))

  import scala.language.postfixOps

  // SELECT "users"."first_name", "users"."last_name" FROM "users" WHERE true and "users"."first_name" is not null
  val withPropertyOp = select(fName, lName).from(users).where(fName isNotNull)
  println(renderRead(withPropertyOp))

  /* SELECT "users"."user_id" FROM "users"
     UNION
     SELECT "orders"."usr_id" FROM "orders"
   */
  val selectWithUnion = select(userId).from(users).union(select(fkUserId).from(orders))

  /* SELECT "users"."user_id" FROM "users"
     UNION ALL
     SELECT "orders"."usr_id" FROM "orders"
   */
  val selectWithUnion = select(userId).from(users).unionAll(select(fkUserId).from(orders))
}
