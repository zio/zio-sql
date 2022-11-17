package zio.sql

import java.util.UUID
import java.time._
import zio.sql.postgresql.PostgresJdbcModule
import zio.schema.DeriveSchema

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

  /*
  insert tuples
  INSERT INTO
    users(uuid, age, date_of_birth, first_name, last_name)
  VALUES
    ('60b01fc9-c902-4468-8d49-3c0f989def37', ‘1983-01-05’, 22, 'Ronald', 'Russell')
   */

  val data         =
    List(
      (UUID.randomUUID(), 22, LocalDate.ofYearDay(1990, 1), "Ronald1", "Russel1"),
      (UUID.randomUUID(), 32, LocalDate.ofYearDay(1980, 1), "Ronald2", "Russel2"),
      (UUID.randomUUID(), 42, LocalDate.ofYearDay(1970, 1), "Ronald3", "Russel3")
    )
  val insertTuples = insertInto(users)(userId, age, dob, fName, lName)
    .values(data)

  val insertedTupleRows = execute(insertTuples)
  println(s"$insertedTupleRows rows are inserted!")

  /*
  insert as case class with schema
  INSERT INTO
    users(uuid, age, date_of_birth, first_name, last_name)
  VALUES
    ('60b01fc9-c902-4468-8d49-3c0f989def37', ‘1983-01-05’, 22, 'Ronald', 'Russell')
   */

  final case class User(
    userId: UUID,
    age: Int,
    dateOfBirth: LocalDate,
    firstName: String,
    lastName: String
  )
  implicit val userSchema = DeriveSchema.gen[User]

  val dataSchema: User = User(UUID.randomUUID(), 22, LocalDate.ofYearDay(1990, 1), "Ronald", "Russel")

  val insertSchema = insertInto(users)(
    userId,
    age,
    dob,
    fName,
    lName
  ).values(dataSchema)

  val insertedSchemaRows = execute(insertSchema)
  println(s"$insertedSchemaRows rows are inserted!")

  /* SELECT "users"."user_id" FROM "users"
     UNION
     SELECT "orders"."usr_id" FROM "orders"
   */
  val selectWithUnion = select(userId).from(users).union(select(fkUserId).from(orders))

  /* SELECT "users"."user_id" FROM "users"
     UNION ALL
     SELECT "orders"."usr_id" FROM "orders"
   */
  val selectWithUnionAll = select(userId).from(users).unionAll(select(fkUserId).from(orders))
}
