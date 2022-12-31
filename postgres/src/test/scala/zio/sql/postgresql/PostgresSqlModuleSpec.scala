package zio.sql.postgresql

import java.math.BigDecimal
import java.time._
import java.util.UUID

import com.github.ghik.silencer.silent
import zio._
import zio.schema._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.sql.Features._

import scala.language.postfixOps

object PostgresSqlModuleSpec extends PostgresRunnableSpec with DbSchema {

  import AggregationDef._
  import CustomerSchema._

  @silent
  private def customerSelectJoseAssertion[F: IsNotAggregated](
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

    for {
      r <- testResult.runCollect
    } yield assert(r)(hasSameElementsDistinct(expected))
  }

  override def specLayered = suite("Postgres module")(
    test("`in` clause sequence") {
      import ProductPrices._

      val query = select(productPricesOrderId).from(productPrices).where(productPrice in List(10, 20, 74))

      for {
        result <- execute(query).runCollect
      } yield assertTrue(
        result == Chunk(
          UUID.fromString("7368abf4-aed2-421f-b426-1725de756895"),
          UUID.fromString("4c770002-4c8f-455a-96ff-36a8186d5290"),
          UUID.fromString("105a2701-ef93-4e25-81ab-8952cc7d9daa")
        )
      )
    },
    test("`in` clause from subquery") {
      import ProductPrices._
      import OrderDetailsSchema._

      val higherPrices = select(productPrice).from(productPrices).where(productPrice > 74)

      val query = select(orderDetailsOrderId).from(orderDetails).where(unitPrice in higherPrices)

      for {
        result <- execute(query).runCollect
      } yield assertTrue(
        result == Chunk(
          UUID.fromString("9473a0bc-396a-4936-96b0-3eea922af36b"),
          UUID.fromString("fd0fa8d4-e1a0-4369-be07-945450db5d36"),
          UUID.fromString("763a7c39-833f-4ee8-9939-e80dfdbfc0fc")
        )
      )
    },
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
      import OrdersSchema._

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

      for {
        r <- execute(query).map(Row tupled _).runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("can do correlated subqueries in selections - counts orders for each customer") {
      import OrdersSchema._

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

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
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

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers) where (verified isNotTrue)
      println(renderDelete(query))

      val result = execute(query)

      for {
        r <- result
      } yield assertTrue(r == 1)
    },
    test("Can delete all from a single table") {
      val query = deleteFrom(customers)
      println(renderDelete(query))

      val result = execute(query)

      for {
        r <- result
      } yield assertTrue(r == 4)
    },
    test("group by can be called on non aggregated collumn") {
      import OrdersSchema._

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

      for {
        actual <- execute(query).map(_.toString()).runCollect.map(_.toList)
      } yield assertTrue(actual == expected)
    },
    test("group by have to be called on column from selection") {
      import OrdersSchema._

      val expected = List(6, 5, 5, 5, 4)

      val query = select(fkCustomerId, Count(orderId))
        .from(orders)
        .groupBy(fkCustomerId)
        .orderBy(Ordering.Desc(Count(orderId)))

      for {
        actual <- execute(query).map(arg => arg._2.toInt).runCollect.map(_.toList)
      } yield assertTrue(actual == expected)
    },
    test("insert - 1 rows into customers") {
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

      val data =
        CustomerRow(UUID.randomUUID(), "Jaro", "Regec", true, LocalDate.ofYearDay(1990, 1), created.toString, created)

      implicit val customerRowSchema =
        Schema.CaseClass7[UUID, String, String, Boolean, LocalDate, String, ZonedDateTime, CustomerRow](
          TypeId.parse("zio.sql.postgresql.PostgresModuleSpec.CustomerRow"),
          Schema.Field(
            "id",
            Schema.primitive[UUID](zio.schema.StandardType.UUIDType),
            get0 = _.id,
            set0 = (r, a) => r.copy(id = a)
          ),
          Schema.Field(
            "firstName",
            Schema.primitive[String](zio.schema.StandardType.StringType),
            get0 = _.firstName,
            set0 = (r, a) => r.copy(firstName = a)
          ),
          Schema.Field(
            "lastName",
            Schema.primitive[String](zio.schema.StandardType.StringType),
            get0 = _.lastName,
            set0 = (r, a) => r.copy(lastName = a)
          ),
          Schema.Field(
            "verified",
            Schema.primitive[Boolean](zio.schema.StandardType.BoolType),
            get0 = _.verified,
            set0 = (r, a) => r.copy(verified = a)
          ),
          Schema.Field(
            "dateOfBirth",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType),
            get0 = _.dateOfBirth,
            set0 = (r, a) => r.copy(dateOfBirth = a)
          ),
          Schema.Field(
            "cretedTimestampString",
            Schema.primitive[String](zio.schema.StandardType.StringType),
            get0 = _.cretedTimestampString,
            set0 = (r, a) => r.copy(cretedTimestampString = a)
          ),
          Schema.Field(
            "createdTimestamp",
            Schema.primitive[ZonedDateTime](
              zio.schema.StandardType.ZonedDateTimeType
            ),
            get0 = _.createdTimestamp,
            set0 = (r, a) => r.copy(createdTimestamp = a)
          ),
          CustomerRow.apply
        )

      val query = insertInto(customers)(
        customerId,
        dob,
        fName,
        lName,
        verified,
        createdString,
        createdTimestamp
      ).values(data)

      for {
        result <- execute(query)
      } yield assertTrue(result == 1)

    },
    test("insert - insert 10 rows into orders") {
      import OrdersSchema._

      val data = List(
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now()),
        Orders(UUID.randomUUID(), UUID.randomUUID(), LocalDate.now())
      )

      val query = insertInto(orders)(orderId, fkCustomerId, orderDate)
        .values(data)

      for {
        result <- execute(query)
      } yield assertTrue(result == 10)
    },
    test("insert - 4 rows into orderDetails") {
      import OrderDetailsSchema._

      case class OrderDetailsRow(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

      implicit val orderDetailsRowSchema = Schema
        .CaseClass4[UUID, UUID, Int, BigDecimal, OrderDetailsRow](
          TypeId.parse("zio.sql.postgresql.PostgresModuleSpec.OrderDetailsRow"),
          Schema.Field(
            "orderId",
            Schema.primitive[UUID](zio.schema.StandardType.UUIDType),
            get0 = _.orderId,
            set0 = (r, a) => r.copy(orderId = a)
          ),
          Schema.Field(
            "productId",
            Schema.primitive[UUID](zio.schema.StandardType.UUIDType),
            get0 = _.productId,
            set0 = (r, a) => r.copy(productId = a)
          ),
          Schema.Field(
            "quantity",
            Schema.primitive[Int](zio.schema.StandardType.IntType),
            get0 = _.quantity,
            set0 = (r, a) => r.copy(quantity = a)
          ),
          Schema.Field(
            "unitPrice",
            Schema.primitive[BigDecimal](zio.schema.StandardType.BigDecimalType),
            get0 = _.unitPrice,
            set0 = (r, a) => r.copy(unitPrice = a)
          ),
          OrderDetailsRow.apply
        )

      val rows = List(
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 4, BigDecimal.valueOf(11.00)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 1, BigDecimal.valueOf(11.00)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 1, BigDecimal.valueOf(11.50)),
        OrderDetailsRow(UUID.randomUUID(), UUID.randomUUID(), 2, BigDecimal.valueOf(10.50))
      )

      val query = insertInto(orderDetails)(orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice)
        .values(rows)

      for {
        result <- execute(query)
      } yield assertTrue(result == 4)
    },
    test("insert into orderDetails with tuples") {
      import OrderDetailsSchema._

      val query = insertInto(orderDetails)(orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice)
        .values((UUID.randomUUID(), UUID.randomUUID(), 4, BigDecimal.valueOf(11.00)))

      for {
        result <- execute(query)
      } yield assertTrue(result == 1)
    },
    test("insert into customers with tuples") {
      import CustomerSchema._

      /**
       * insert into customers
       *              (id, first_name, last_name, verified, dob, created_timestamp_string, created_timestamp)
       *          values
       *              ('60b01fc9-c902-4468-8d49-3c0f989def37', 'Ronald', 'Russell', true, '1983-01-05', '2020-11-21T19:10:25+00:00', '2020-11-21 19:10:25+00'),
       */

      val created = ZonedDateTime.now()

      val row =
        ((UUID.randomUUID(), LocalDate.ofYearDay(1990, 1), "Jaro", "Regec", true, created.toString, created))

      val query = insertInto(customers)(
        customerId,
        dob,
        fName,
        lName,
        verified,
        createdString,
        createdTimestamp
      ).values(row)

      for {
        result <- execute(query)
      } yield assertTrue(result == 1)
    },
    test("insert into products") {
      import ProductSchema._

      val tupleData = List(
        (UUID.randomUUID(), "product 1", "product desription", "image url"),
        (UUID.randomUUID(), "product 2", "product desription", "image url"),
        (UUID.randomUUID(), "product 3", "product desription", "image url"),
        (UUID.randomUUID(), "product 4", "product desription", "image url")
      )

      val query = insertInto(products)(productId, productName, description, imageURL).values(tupleData)

      for {
        result <- execute(query)
      } yield assertTrue(result == 4)
    },
    test("insert and query nullable field") {
      import PersonsSchema._

      val expected = List(
        (Some("Russell"), Some(LocalDate.of(1983, 1, 5))),
        (Some("Noel"), None),
        (Some("Paterso"), Some(LocalDate.of(1990, 11, 16))),
        (Some("Murray"), Some(LocalDate.of(1995, 11, 12))),
        (None, None),
        (Some("Harvey"), Some(LocalDate.of(2022, 1, 31))),
        (Some("Dent"), None),
        (Some("Charles"), None)
      )

      val insertSome = insertInto(persons)(personsId, personsName, birthDate)
        .values((UUID.randomUUID(), Option.apply("Harvey"), Option.apply(LocalDate.of(2022, 1, 31))))

      val insertNone =
        insertInto(persons)(personsId, personsName, birthDate).values((UUID.randomUUID(), Some("Dent"), None))

      val insertNone2 = insertInto(persons)(personsId, personsName, birthDate)
        .values(Persons(UUID.randomUUID(), Some("Charles"), None))

      for {
        _       <- execute(insertSome)
        _       <- execute(insertNone)
        _       <- execute(insertNone2)
        persons <- execute(select(personsName, birthDate).from(persons)).runCollect
      } yield assertTrue(persons.toList == expected)
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

      val expected =
        Chunk(
          ("Métro de Paris", "Paris", 16L),
          ("Chongqing Metro", "Chongqing", 4L),
          ("Metro Warszawskie", "Warszawa", 2L)
        )

      for {
        result <- execute(complexQuery).runCollect
      } yield assertTrue(result == expected)

    },
    test("update rows") {
      import PersonsSchema._

      // TODO support here also Some and None
      for {
        result <- execute(update(persons).set(personsName, Option("Charlie")).where(personsName === Option("Murray")))
      } yield assertTrue(result == 1)
    }
  ) @@ sequential
}
