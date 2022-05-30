package zio.sql.postgresql

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.time._
import java.util.UUID
import scala.language.postfixOps
import zio.schema.Schema
import java.time.format.DateTimeFormatter

object PostgresSqlModuleSpec extends PostgresRunnableSpec with DbSchema {

  import AggregationDef._
  import Customers._
  import Orders._

  private def customerSelectJoseAssertion[F: Features.IsNotAggregated](
    condition: Expr[F, customers.TableType, Boolean]
  ) = {
    case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

    val query =
      select(customerId, fName, lName, verified, dob).from(customers).where(condition)

    val expected =
      Seq(
        Customer(
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
          "Jose",
          "Wiggins",
          false,
          LocalDate.parse("1987-03-23")
        )
      )

    val testResult = execute(query).map { case row =>
      Customer(row._1, row._2, row._3, row._4, row._5)
    }

    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r)(hasSameElementsDistinct(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

  override def specLayered = suite("Postgres module")(
    test("Can select from single table") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob).from(customers)

      val expected =
        Seq(
          Customer(
            UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
            "Ronald",
            "Russell",
            LocalDate.parse("1983-01-05")
          ),
          Customer(
            UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"),
            "Terrence",
            "Noel",
            LocalDate.parse("1999-11-02")
          ),
          Customer(
            UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"),
            "Mila",
            "Paterso",
            LocalDate.parse("1990-11-16")
          ),
          Customer(
            UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
            "Alana",
            "Murray",
            LocalDate.parse("1995-11-12")
          ),
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can select with property unary operator") {
      customerSelectJoseAssertion(verified isNotTrue)
    },
    test("Can select with property binary operator with UUID") {
      customerSelectJoseAssertion(customerId === UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"))
    },
    test("Can select with property binary operator with String") {
      customerSelectJoseAssertion(fName === "Jose")
    },
    test("Can select with property binary operator with LocalDate") {
      customerSelectJoseAssertion(dob === LocalDate.parse("1987-03-23"))
    },
    test("Can select with property binary operator with LocalDateTime") {
      customerSelectJoseAssertion(dob === LocalDateTime.parse("1987-03-23T00:00:00"))
    },
    test("Can select with property binary operator with OffsetDateTime") {
      customerSelectJoseAssertion(dob === OffsetDateTime.parse("1987-03-23T00:00:00Z"))
    },
    test("Can select with property binary operator with ZonedLocalDate") {
      customerSelectJoseAssertion(dob === ZonedDateTime.parse("1987-03-23T00:00:00Z"))
    },
    test("Can select with property binary operator with Instant") {
      customerSelectJoseAssertion(dob === Instant.parse("1987-03-23T00:00:00Z"))
    },
    // TODO try to translate money as "::numeric"
//    test("Can select with property binary operator with numbers") {
//      case class OrderDetails(orderId: UUID, product_id: UUID, quantity: Int, unitPrice: BigDecimal)
//
//      val orderDetailQuantity  = 3
//      val orderDetailUnitPrice = BigDecimal(80.0)
//      val condition            = (quantity === orderDetailQuantity) && (unitPrice === orderDetailUnitPrice)
//      val query                =
//        select(fkOrderId ++ fkProductId ++ quantity ++ unitPrice).from(orderDetails).where(condition)
//
//      println(renderRead(query))
//
//      val expected =
//        Seq(
//          OrderDetails(
//            UUID.fromString("763a7c39-833f-4ee8-9939-e80dfdbfc0fc"),
//            UUID.fromString("105a2701-ef93-4e25-81ab-8952cc7d9daa"),
//            orderDetailQuantity,
//            orderDetailUnitPrice
//          )
//        )
//
//      val testResult = execute(query.to[UUID, UUID, Int, BigDecimal, OrderDetails] { case row =>
//        OrderDetails(row._1, row._2, row._3, row._4)
//      })
//
//      val assertion = for {
//        r <- testResult.runCollect
//      } yield assert(r)(hasSameElementsDistinct(expected))
//
//      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
//    },
    test("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob).from(customers).limit(1).offset(1).orderBy(fName)

      val expected =
        Seq(
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(query).map(Customer tupled _)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can count rows") {
      val query = select(Count(customerId)).from(customers)

      val expected = 5L

      val result = execute(query)

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    test("Can select from joined tables (inner join)") {
      val query = select(fName, lName, orderDate).from(customers.join(orders).on(fkCustomerId === customerId))

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = List(
        ("Ronald", "Russell", LocalDate.parse("2019-03-25")),
        ("Ronald", "Russell", LocalDate.parse("2018-06-04")),
        ("Alana", "Murray", LocalDate.parse("2019-08-19")),
        ("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
        ("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
        ("Ronald", "Russell", LocalDate.parse("2020-03-19")),
        ("Alana", "Murray", LocalDate.parse("2020-05-11")),
        ("Alana", "Murray", LocalDate.parse("2019-02-21")),
        ("Ronald", "Russell", LocalDate.parse("2018-05-06")),
        ("Mila", "Paterso", LocalDate.parse("2019-02-11")),
        ("Terrence", "Noel", LocalDate.parse("2019-10-12")),
        ("Ronald", "Russell", LocalDate.parse("2019-01-29")),
        ("Terrence", "Noel", LocalDate.parse("2019-02-10")),
        ("Ronald", "Russell", LocalDate.parse("2019-09-27")),
        ("Alana", "Murray", LocalDate.parse("2018-11-13")),
        ("Jose", "Wiggins", LocalDate.parse("2020-01-15")),
        ("Terrence", "Noel", LocalDate.parse("2018-07-10")),
        ("Mila", "Paterso", LocalDate.parse("2019-08-01")),
        ("Alana", "Murray", LocalDate.parse("2019-12-08")),
        ("Mila", "Paterso", LocalDate.parse("2019-11-04")),
        ("Mila", "Paterso", LocalDate.parse("2018-10-14")),
        ("Terrence", "Noel", LocalDate.parse("2020-04-05")),
        ("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
        ("Terrence", "Noel", LocalDate.parse("2019-05-14")),
        ("Mila", "Paterso", LocalDate.parse("2020-04-30"))
      )

      val result = execute(query)

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can do lateral join") {
      import PostgresSpecific.PostgresSpecificTable._

      /**
       *  select customers.id, customers.first_name, customers.last_name, derived.order_date
       *          from customers,
       *          lateral  (
       *              select orders.order_date
       *              from orders
       *              where customers.id = orders.customer_id
       *              order by orders.order_date desc limit 1 ) derived order by derived.order_date desc
       */

      case class Row(id: UUID, firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(
        Row(UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"), "Alana", "Murray", LocalDate.parse("2020-05-11")),
        Row(UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"), "Mila", "Paterso", LocalDate.parse("2020-04-30")),
        Row(UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"), "Terrence", "Noel", LocalDate.parse("2020-04-05")),
        Row(
          UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"),
          "Ronald",
          "Russell",
          LocalDate.parse("2020-03-19")
        ),
        Row(UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"), "Jose", "Wiggins", LocalDate.parse("2020-01-15"))
      )

      import DerivedTables._

      val query =
        select(customerId, fName, lName, orderDateDerived)
          .from(customers.lateral(orderDateDerivedTable))
          .orderBy(Ordering.Desc(orderDateDerived))

      val result = execute(query).map(Row tupled _)

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("can do correlated subqueries in selections - counts orders for each customer") {

      /**
       * select first_name, last_name, (
       *    select count(orders.id) from orders
       *    where customers.id = orders.customer_id
       * ) as "count"
       * from customers
       */

      case class Row(firstName: String, lastName: String, count: Long)

      val expected = Seq(
        Row("Alana", "Murray", 5),
        Row("Mila", "Paterso", 5),
        Row("Terrence", "Noel", 5),
        Row("Ronald", "Russell", 6),
        Row("Jose", "Wiggins", 4)
      )

      val subquery =
        customers.subselect(Count(orderId)).from(orders).where(fkCustomerId === customerId)

      val query = select(fName, lName, (subquery as "Count")).from(customers)

      val result = execute(query).map { case row =>
        Row(row._1, row._2, row._3)
      }

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can select using like") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob).from(customers).where(fName like "Jo%")

      val expected = Seq(
        Customer(
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
          "Jose",
          "Wiggins",
          LocalDate.parse("1987-03-23")
        )
      )

      val testResult = execute(query).map { case (uuid, firstName, lastName, dob) =>
        Customer(uuid, firstName, lastName, dob)
      }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers) where (verified isNotTrue)
      println(renderDelete(query))

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(1))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can delete all from a single table") {
      val query = deleteFrom(customers)
      println(renderDelete(query))

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(4))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("group by can be called on non aggregated collumn") {

      /**
       *        select customer_id
       *          from orders
       *          group by customer_id
       */

      val expected = List(
        "636ae137-5b1a-4c8c-b11f-c47c624d9cdc",
        "60b01fc9-c902-4468-8d49-3c0f989def37",
        "df8215a2-d5fd-4c6c-9984-801a1b3a2a0b",
        "784426a5-b90a-4759-afbb-571b7a0ba35e",
        "f76c9ace-be07-4bf3-bd4c-4a9c62882e64"
      )

      val query = select(fkCustomerId)
        .from(orders)
        .groupBy(fkCustomerId)

      val actual = execute(query).map(_.toString()).runCollect.map(_.toList)

      assertZIO(actual)(equalTo(expected))
    },
    test("group by have to be called on column from selection") {

      /**
       *        select customer_id, count(id)
       *          from orders
       *          group by customer_id
       *          order by count(id) desc
       */

      import AggregationDef._

      val expected = List(6, 5, 5, 5, 4)

      val query = select(fkCustomerId, Count(orderId))
        .from(orders)
        .groupBy(fkCustomerId)
        .orderBy(Ordering.Desc(Count(orderId)))

      val actual = execute(query).map(arg => arg._2.toInt).runCollect.map(_.toList)

      assertZIO(actual)(equalTo(expected))
    },
    test("insert - 1 rows into customers") {

      /**
       * insert into customers
       *              (id, first_name, last_name, verified, dob, created_timestamp_string, created_timestamp)
       *          values
       *              ('60b01fc9-c902-4468-8d49-3c0f989def37', 'Ronald', 'Russell', true, '1983-01-05', '2020-11-21T19:10:25+00:00', '2020-11-21 19:10:25+00'))
       */

      final case class CustomerRow(
        id: UUID,
        firstName: String,
        lastName: String,
        verified: Boolean,
        dateOfBirth: LocalDate,
        cretedTimestampString: String,
        createdTimestamp: ZonedDateTime
      )

      val created = ZonedDateTime.now()
      import java.time._

      implicit val customerRowSchema =
        Schema.CaseClass7[UUID, String, String, Boolean, LocalDate, String, ZonedDateTime, CustomerRow](
          Schema.Field("id", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
          Schema.Field("firstName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("lastName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("verified", Schema.primitive[Boolean](zio.schema.StandardType.BoolType)),
          Schema.Field(
            "localDate",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))
          ),
          Schema.Field("cretedTimestampString", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field(
            "createdTimestamp",
            Schema.primitive[ZonedDateTime](
              zio.schema.StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME)
            )
          ),
          CustomerRow.apply,
          _.id,
          _.firstName,
          _.lastName,
          _.verified,
          _.dateOfBirth,
          _.cretedTimestampString,
          _.createdTimestamp
        )

      val data =
        CustomerRow(UUID.randomUUID(), "Jaro", "Regec", true, LocalDate.ofYearDay(1990, 1), created.toString, created)

      val query = insertInto(customers)(
        customerId,
        fName,
        lName,
        verified,
        dob,
        createdString,
        createdTimestamp
      ).values(data)

      assertZIO(execute(query))(equalTo(1))
    },
    test("insert - insert 10 rows into orders") {

      /**
       *       insert into product_prices
       *            (product_id, effective, price)
       *       values
       *            ('7368ABF4-AED2-421F-B426-1725DE756895', '2018-01-01', 10.00),
       *            ('7368ABF4-AED2-421F-B426-1725DE756895', '2019-01-01', 11.00),
       *            ('D5137D3A-894A-4109-9986-E982541B434F', '2020-01-01', 55.00),
       *            .....
       *            ('D5137D3A-894A-4109-9986-E982541B43BB', '2020-01-01', 66.00);
       */

      final case class InputOrders(uuid: UUID, customerId: UUID, localDate: LocalDate)

      implicit val inputOrdersSchema = Schema.CaseClass3[UUID, UUID, LocalDate, InputOrders](
        Schema.Field("uuid", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field("customerId", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field(
          "localDate",
          Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))
        ),
        InputOrders.apply,
        _.uuid,
        _.customerId,
        _.localDate
      )

      val data = List(
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        InputOrders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now())
      )

      val query = insertInto(orders)(orderId, fkCustomerId, orderDate)
        .values(data)

      assertZIO(execute(query))(equalTo(10))
    },
    test("insert - 4 rows into orderDetails") {

      /**
       * insert into order_details
       *            (order_id, product_id, quantity, unit_price)
       *        values
       *            ('9022DD0D-06D6-4A43-9121-2993FC7712A1', '7368ABF4-AED2-421F-B426-1725DE756895', 4, 11.00),
       *            ('38D66D44-3CFA-488A-AC77-30277751418F', '7368ABF4-AED2-421F-B426-1725DE756895', 1, 11.00),
       *            ('7B2627D5-0150-44DF-9171-3462E20797EE', '7368ABF4-AED2-421F-B426-1725DE756895', 1, 11.50),
       *            ('62CD4109-3E5D-40CC-8188-3899FC1F8BDF', '7368ABF4-AED2-421F-B426-1725DE756895', 2, 10.90),
       */

      import OrderDetails._

      case class OrderDetailsRow(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

      // TODO we need schema for scala.math.BigDecimal. Probably directly in zio-schema ?
      implicit val bigDecimalSchema: Schema[BigDecimal] =
        Schema[java.math.BigDecimal]
          .transform(bigDec => new BigDecimal(bigDec, java.math.MathContext.DECIMAL128), _.bigDecimal)

      implicit val orderDetailsRowSchema = Schema.CaseClass4[UUID, UUID, Int, BigDecimal, OrderDetailsRow](
        Schema.Field("orderId", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field("productId", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field("quantity", Schema.primitive[Int](zio.schema.StandardType.IntType)),
        Schema.Field("unitPrice", bigDecimalSchema),
        OrderDetailsRow.apply,
        _.orderId,
        _.productId,
        _.quantity,
        _.unitPrice
      )

      val rows = List(
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 4, BigDecimal.valueOf(11.00)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 1, BigDecimal.valueOf(11.00)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 1, BigDecimal.valueOf(11.50)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 2, BigDecimal.valueOf(10.50))
      )

      val query = insertInto(orderDetails)(orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice)
        .values(rows)

      assertZIO(execute(query))(equalTo(4))
    },
    test("insert into orderDetails with tuples") {

      /**
       * insert into order_details
       *            (order_id, product_id, quantity, unit_price)
       *        values
       *            ('9022DD0D-06D6-4A43-9121-2993FC7712A1', '7368ABF4-AED2-421F-B426-1725DE756895', 4, 11.00))
       */

      import OrderDetails._

      val query = insertInto(orderDetails)(orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice)
        .values((UUID.randomUUID(), UUID.randomUUID(), 4, BigDecimal.valueOf(11.00)))

      assertZIO(execute(query))(equalTo(1))
    },
    test("insert into customers with tuples") {

      /**
       * insert into customers
       *              (id, first_name, last_name, verified, dob, created_timestamp_string, created_timestamp)
       *          values
       *              ('60b01fc9-c902-4468-8d49-3c0f989def37', 'Ronald', 'Russell', true, '1983-01-05', '2020-11-21T19:10:25+00:00', '2020-11-21 19:10:25+00'),
       */

      val created = ZonedDateTime.now()

      val row =
        ((UUID.randomUUID(), "Jaro", "Regec", true, LocalDate.ofYearDay(1990, 1), created.toString, created))

      val query = insertInto(customers)(
        customerId,
        fName,
        lName,
        verified,
        dob,
        createdString,
        createdTimestamp
      ).values(row)

      assertZIO(execute(query))(equalTo(1))
    },
    test("insert into products") {
      import Products._

      val tupleData = List(
        (UUID.randomUUID(), "product 1", "product desription", "image url"),
        (UUID.randomUUID(), "product 2", "product desription", "image url"),
        (UUID.randomUUID(), "product 3", "product desription", "image url"),
        (UUID.randomUUID(), "product 4", "product desription", "image url")
      )

      val query = insertInto(products)(productId, productName, description, imageURL).values(tupleData)

      assertZIO(execute(query))(equalTo(4))
    },
    test("insert and query nullable field") {
      import Persons._

      val query = select(fName, lName, dob)
        .from(persons)

      final case class Person(id: UUID, firstName: String, lastName: String, dateOfBirth: Option[LocalDate])

      implicit val localDateSchema = Schema.option[LocalDate](
        Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))
      )

      implicit val personSchema = Schema.CaseClass4[UUID, String, String, Option[LocalDate], Person](
        Schema.Field("id", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field("firstName", Schema.primitive[String](zio.schema.StandardType.StringType)),
        Schema.Field("lastName", Schema.primitive[String](zio.schema.StandardType.StringType)),
        Schema.Field("dateOfBirth", localDateSchema),
        Person.apply,
        _.id,
        _.firstName,
        _.lastName,
        _.dateOfBirth
      )

      val personValue = Person(UUID.randomUUID(), "Charles", "Harvey", None)

      val insertSome = insertInto(persons)(personId, fName, lName, dob)
        .values((UUID.randomUUID(), "Charles", "Dent", Some(LocalDate.of(2022, 1, 31))))

      val insertNone = insertInto(persons)(personId, fName, lName, dob)
        .values((UUID.randomUUID(), "Martin", "Harvey", None))

      val insertNone2 = insertInto(persons)(personId, fName, lName, dob)
        .values(personValue)

      val result = for {
        _       <- execute(insertSome)
        _       <- execute(insertNone)
        _       <- execute(insertNone2)
        persons <- execute(query).runCollect
      } yield (persons.toList)

      val expected = List(
        ("Ronald", "Russell", Some(LocalDate.of(1983, 1, 5))),
        ("Terrence", "Noel", None),
        ("Mila", "Paterso", Some(LocalDate.of(1990, 11, 16))),
        ("Alana", "Murray", Some(LocalDate.of(1995, 11, 12))),
        ("Jose", null, None),
        ("Charles", "Dent", Some(LocalDate.of(2022, 1, 31))),
        ("Martin", "Harvey", None),
        ("Charles", "Harvey", None)
      )

      assertZIO(result)(equalTo(expected))
    },
    test("in joined tables, columns of the same name from different table are treated as different columns") {
      import Cities._
      import Ordering._

      /**
       *          SELECT ms.name, c.name, COUNT(ml.id) as line_count
       *            FROM metro_line as ml
       *            JOIN metro_system as ms on ml.system_id = ms.id
       *            JOIN city AS c ON ms.city_id = c.id
       *          GROUP BY ms.name, c.name
       *          ORDER BY line_count DESC
       */
      val lineCount = (Count(metroLineId) as "line_count")

      val complexQuery = select(metroSystemName, cityName, lineCount)
        .from(
          metroLine
            .join(metroSystem)
            .on(metroSystemId === systemId)
            .join(city)
            .on(cityIdFk === cityId)
        )
        .groupBy(metroSystemName, cityName)
        .orderBy(Desc(lineCount))

      val result = execute(complexQuery).runCollect

      val expected =
        Chunk(
          ("Métro de Paris", "Paris", 16L),
          ("Chongqing Metro", "Chongqing", 4L),
          ("Metro Warszawskie", "Warszawa", 2L)
        )

      assertZIO(result)(equalTo(expected))
    },
    test("update rows") {
      val query = update(customers).set(fName, "Jaroslav").where(fName === "Jaro")

      assertZIO(execute(query))(equalTo(2))
    }
  ) @@ sequential
}
