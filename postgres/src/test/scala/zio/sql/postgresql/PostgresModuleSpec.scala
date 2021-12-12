package zio.sql.postgresql

import zio._
import zio.test.Assertion._
import zio.test.TestAspect.sequential
import zio.test._

import java.time._
import java.util.UUID
import scala.language.postfixOps
import zio.schema.Schema
import java.time.format.DateTimeFormatter

object PostgresModuleSpec extends PostgresRunnableSpec with ShopSchema {

  import AggregationDef._
  import Customers._
  import Orders._

  private def customerSelectJoseAssertion(condition: Expr[_, customers.TableType, Boolean]) = {
    case class Customer(id: UUID, fname: String, lname: String, verified: Boolean, dateOfBirth: LocalDate)

    val query =
      select(customerId ++ fName ++ lName ++ verified ++ dob).from(customers).where(condition)

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

    val testResult = execute(
      query
        .to[UUID, String, String, Boolean, LocalDate, Customer] { case row =>
          Customer(row._1, row._2, row._3, row._4, row._5)
        }
    )

    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r)(hasSameElementsDistinct(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

  override def specLayered = suite("Postgres module")(
    testM("Can select from single table") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob).from(customers)

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

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select with property unary operator") {
      customerSelectJoseAssertion(verified isNotTrue)
    },
    testM("Can select with property binary operator with UUID") {
      customerSelectJoseAssertion(customerId === UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"))
    },
    testM("Can select with property binary operator with String") {
      customerSelectJoseAssertion(fName === "Jose")
    },
    testM("Can select with property binary operator with LocalDate") {
      customerSelectJoseAssertion(dob === LocalDate.parse("1987-03-23"))
    },
    testM("Can select with property binary operator with LocalDateTime") {
      customerSelectJoseAssertion(dob === LocalDateTime.parse("1987-03-23T00:00:00"))
    },
    testM("Can select with property binary operator with OffsetDateTime") {
      customerSelectJoseAssertion(dob === OffsetDateTime.parse("1987-03-23T00:00:00Z"))
    },
    testM("Can select with property binary operator with ZonedLocalDate") {
      customerSelectJoseAssertion(dob === ZonedDateTime.parse("1987-03-23T00:00:00Z"))
    },
    testM("Can select with property binary operator with Instant") {
      customerSelectJoseAssertion(dob === Instant.parse("1987-03-23T00:00:00Z"))
    },
    //TODO try to translate money as "::numeric"
//    testM("Can select with property binary operator with numbers") {
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
    testM("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob).from(customers).limit(1).offset(1).orderBy(fName)

      val expected =
        Seq(
          Customer(
            UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
            "Jose",
            "Wiggins",
            LocalDate.parse("1987-03-23")
          )
        )

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can count rows") {
      val query = select(Count(customerId)).from(customers)

      val expected = 5L

      val result = execute(query.to[Long, Long](identity))

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    testM("Can select from joined tables (inner join)") {
      val query = select(fName ++ lName ++ orderDate).from(customers.join(orders).on(fkCustomerId === customerId))

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(
        Row("Ronald", "Russell", LocalDate.parse("2019-03-25")),
        Row("Ronald", "Russell", LocalDate.parse("2018-06-04")),
        Row("Alana", "Murray", LocalDate.parse("2019-08-19")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
        Row("Ronald", "Russell", LocalDate.parse("2020-03-19")),
        Row("Alana", "Murray", LocalDate.parse("2020-05-11")),
        Row("Alana", "Murray", LocalDate.parse("2019-02-21")),
        Row("Ronald", "Russell", LocalDate.parse("2018-05-06")),
        Row("Mila", "Paterso", LocalDate.parse("2019-02-11")),
        Row("Terrence", "Noel", LocalDate.parse("2019-10-12")),
        Row("Ronald", "Russell", LocalDate.parse("2019-01-29")),
        Row("Terrence", "Noel", LocalDate.parse("2019-02-10")),
        Row("Ronald", "Russell", LocalDate.parse("2019-09-27")),
        Row("Alana", "Murray", LocalDate.parse("2018-11-13")),
        Row("Jose", "Wiggins", LocalDate.parse("2020-01-15")),
        Row("Terrence", "Noel", LocalDate.parse("2018-07-10")),
        Row("Mila", "Paterso", LocalDate.parse("2019-08-01")),
        Row("Alana", "Murray", LocalDate.parse("2019-12-08")),
        Row("Mila", "Paterso", LocalDate.parse("2019-11-04")),
        Row("Mila", "Paterso", LocalDate.parse("2018-10-14")),
        Row("Terrence", "Noel", LocalDate.parse("2020-04-05")),
        Row("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
        Row("Terrence", "Noel", LocalDate.parse("2019-05-14")),
        Row("Mila", "Paterso", LocalDate.parse("2020-04-30"))
      )

      val result = execute(
        query
          .to[String, String, LocalDate, Row] { case row =>
            Row(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can do lateral join") {
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
        select(customerId ++ fName ++ lName ++ orderDateDerived)
          .from(customers.lateral(orderDateDerivedTable))
          .orderBy(Ordering.Desc(orderDateDerived))

      val result = execute(
        query
          .to[UUID, String, String, LocalDate, Row] { case row =>
            Row(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("can do correlated subqueries in selections - counts orders for each customer") {

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

      val query = select(fName ++ lName ++ (subquery as "Count")).from(customers)

      val result = execute(
        query
          .to[String, String, Long, Row] { case row =>
            Row(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can select using like") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob).from(customers).where(fName like "Jo%")

      val expected = Seq(
        Customer(
          UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"),
          "Jose",
          "Wiggins",
          LocalDate.parse("1987-03-23")
        )
      )

      val testResult = execute(
        query
          .to[UUID, String, String, LocalDate, Customer] { case row =>
            Customer(row._1, row._2, row._3, row._4)
          }
      )

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can delete from single table with a condition") {
      val query = deleteFrom(customers) where (verified isNotTrue)
      println(renderDelete(query))

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(1))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Can delete all from a single table") {
      val query = deleteFrom(customers)
      println(renderDelete(query))

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(4))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("simple insert - inserted 1 row") {

      /**
       * insert into
       *     customers
       *        (id, first_name, last_name, verifier, dob, created_timestamp_string, created_timestamp)
       * values
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec', true, 1999-11-02, 2020-11-21T19:10:25+00:00, '2020-11-21 19:10:25+00')
       */

      val dobValue = LocalDate.now()
      val created  = ZonedDateTime.now()

      val query = insertAltInto(customers)
        .values(
          (customerId         -> java.util.UUID.fromString("0511474d-8eed-4307-bdb0-e39a561205b6")) ++
            (fName            -> "Jaro") ++
            (lName            -> "Regec") ++
            (verified         -> false) ++
            (dob              -> dobValue) ++
            (createdString    -> created.toString) ++
            (createdTimestamp -> created)
        )

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(1))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("insert - 2 rows into cutomers") {

      /**
       * insert into customers
       *              (id, first_name, last_name, verified, dob, created_timestamp_string, created_timestamp)
       *          values
       *              ('60b01fc9-c902-4468-8d49-3c0f989def37', 'Ronald', 'Russell', true, '1983-01-05', '2020-11-21T19:10:25+00:00', '2020-11-21 19:10:25+00'),
       *              ('f76c9ace-be07-4bf3-bd4c-4a9c62882e64', 'Terrence', 'Noel', true, '1999-11-02', '2020-11-21T15:10:25-04:00', '2020-11-21 15:10:25-04'),
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

      implicit val customerRowSchema = // DeriveSchema.gen[CustomerRow]
        Schema.CaseClass7[UUID, String, String, Boolean, LocalDate, String, ZonedDateTime, CustomerRow](
          Chunk.empty,
          Schema.Field("id", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
          Schema.Field("firstName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("lastName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("verified", Schema.primitive[Boolean](zio.schema.StandardType.BoolType)),
          Schema.Field(
            "localDate",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDate(DateTimeFormatter.ISO_DATE))
          ),
          Schema.Field("cretedTimestampString", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field(
            "createdTimestamp",
            Schema.primitive[ZonedDateTime](
              zio.schema.StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)
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

      val rows = List(
        CustomerRow(UUID.randomUUID(), "Jaro", "Regec", true, LocalDate.ofYearDay(1990, 1), created.toString, created),
        CustomerRow(
          UUID.randomUUID(),
          "Martin",
          "Mrkva",
          false,
          LocalDate.ofYearDay(1980, 1),
          created.toString,
          created
        )
      )

      val query = insertInto(customers)(
        customerId ++ fName ++ lName ++ verified ++ dob ++ createdString ++ createdTimestamp
      ).values(rows)

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(2))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("insert - insert 10 rows into orders") {

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

      //TODO why does DeriveSchema.gen[T] does not work?
      implicit val inputOrdersSchema = Schema.CaseClass3[UUID, UUID, LocalDate, InputOrders](
        Chunk.empty,
        Schema.Field("uuid", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field("customerId", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
        Schema.Field(
          "localDate",
          Schema.primitive[LocalDate](zio.schema.StandardType.LocalDate(DateTimeFormatter.ISO_DATE))
        ),
        InputOrders.apply,
        _.uuid,
        _.customerId,
        _.localDate
      )

      val orderValues = List(
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

      val query = insertInto(orders)(orderId ++ fkCustomerId ++ orderDate)
        .values(orderValues)

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(10))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("insert - 4 rows into orderDetails") {

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

      //TODO we need schema for scala.math.BigDecimal
      implicit val bigDecimalSchema: Schema[BigDecimal] =
        Schema.Transform(
          Schema.primitive[java.math.BigDecimal](zio.schema.StandardType.BigDecimalType),
          (bigDec: java.math.BigDecimal) => Right(new BigDecimal(bigDec, java.math.MathContext.DECIMAL128)),
          bigDec => Right(bigDec.bigDecimal),
          Chunk.empty
        )

      implicit val orderDetailsRowSchema = Schema.CaseClass4[UUID, UUID, Int, BigDecimal, OrderDetailsRow](
        Chunk.empty,
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

      val query = insertInto(orderDetails)(orderDetailsOrderId ++ orderDetailsProductId ++ quantity ++ unitPrice)
        .values(rows)

      val result = execute(query)

      val assertion = for {
        r <- result
      } yield assert(r)(equalTo(4))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
