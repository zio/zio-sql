package zio.sql.sqlserver

import zio._
import zio.test.Assertion._
import zio.test.TestAspect.{ retries, samples, sequential, shrinks }
import zio.test._

import java.time._
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.language.postfixOps
import zio.schema.DeriveSchema

import java.math.{ BigDecimal, RoundingMode }

object SqlServerModuleSpec extends SqlServerRunnableSpec {

  import AggregationDef._
  import CustomerSchema._

  final case class CustomerRow(
    id: UUID,
    firstName: String,
    lastName: String,
    verified: Boolean,
    dateOfBirth: LocalDate
  )

  implicit val customerRow = DeriveSchema.gen[CustomerRow]

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

    val testResult = execute(query).map { row =>
      Customer(row._1, row._2, row._3, row._4, row._5)
    }

    for {
      r <- testResult.runCollect
    } yield assert(r)(hasSameElementsDistinct(expected))
  }

  override def specLayered = suite("MSSQL Server module")(
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

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Can select with property unary operator") {
      customerSelectJoseAssertion(verified isNotTrue)
    },
    test("Can select with property binary operator with Boolean") {
      customerSelectJoseAssertion(verified === false)
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
    test("Can select with mutltiple binary operators") {
      customerSelectJoseAssertion(verified === false && fName === "Jose" && lName === "Wiggins")
    },
    test("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId, fName, lName, dob).from(customers).limit(1).offset(1).orderBy(fName)

      val expected =
        Seq(
          Customer(
            UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
            "Alana",
            "Murray",
            LocalDate.parse("1995-11-12")
          )
        )

      val testResult = execute(query).map { case row =>
        Customer(row._1, row._2, row._3, row._4)
      }

      for {
        r <- testResult.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Can count rows") {
      val query = select(Count(customerId)).from(customers)

      val expected = 5L

      val result = execute(query)

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    test("correlated subqueries in selections - counts orders for each customer") {

      import OrderSchema._

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
    test("subquery in where clause") {
      import SqlServerSpecific.SqlServerFunctionDef._
      import OrderDetails._
      import ProductSchema._

      /**
       * select order_id, product_id, unit_price from order_details
       * where unit_price > (select AVG(price)
       *                                       from product_prices )
       */

      val query = select(orderDetailsId, productId, unitPrice)
        .from(orderDetails)
        .where(
          unitPrice > select(Avg(price)).from(productPrices)
        )

      case class Row(orderId: UUID, productId: UUID, unitPrice: BigDecimal)

      object Row {
        def create(orderId: String, productId: String, unitPrice: BigDecimal): Row =
          new Row(UUID.fromString(orderId), UUID.fromString(productId), unitPrice.setScale(4, RoundingMode.CEILING))
      }

      val expected = Seq(
        Row.create(
          "04912093-CC2E-46AC-B64C-1BD7BB7758C3",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "9022DD0D-06D6-4A43-9121-2993FC7712A1",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "38D66D44-3CFA-488A-AC77-30277751418F",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "7B2627D5-0150-44DF-9171-3462E20797EE",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "62CD4109-3E5D-40CC-8188-3899FC1F8BDF",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(72.7200)
        ),
        Row.create(
          "9473A0BC-396A-4936-96B0-3EEA922AF36B",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(80.0000)
        ),
        Row.create(
          "B8BAC18D-769F-48ED-809D-4B6C0E4D1795",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(67.2700)
        ),
        Row.create(
          "BEBBFE4D-4EC3-4389-BDC2-50E9EAC2B15B",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(67.2700)
        ),
        Row.create(
          "742D45A0-E81A-41CE-95AD-55B4CABBA258",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(67.2700)
        ),
        Row.create(
          "618AA21F-700B-4CA7-933C-67066CF4CD97",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "606DA090-DD33-4A77-8746-6ED0E8443AB2",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(67.2700)
        ),
        Row.create(
          "FD0FA8D4-E1A0-4369-BE07-945450DB5D36",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(80.0000)
        ),
        Row.create(
          "876B6034-B33C-4497-81EE-B4E8742164C2",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "91CAA28A-A5FE-40D7-979C-BD6A128D0418",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "2C3FC180-D0DF-4D7B-A271-E6CCD2440393",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(70.0000)
        ),
        Row.create(
          "763A7C39-833F-4EE8-9939-E80DFDBFC0FC",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(80.0000)
        ),
        Row.create(
          "5011D206-8EFF-42C4-868E-F1A625E1F186",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "0A48FFB0-EC61-4147-AF56-FC4DBCA8DE0A",
          "105A2701-EF93-4E25-81AB-8952CC7D9DAA",
          BigDecimal.valueOf(74.0000)
        ),
        Row.create(
          "A243FA42-817A-44EC-8B67-22193D212D82",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(45.4500)
        ),
        Row.create(
          "62CD4109-3E5D-40CC-8188-3899FC1F8BDF",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(50.0000)
        ),
        Row.create(
          "9473A0BC-396A-4936-96B0-3EEA922AF36B",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(55.0000)
        ),
        Row.create(
          "852E2DC9-4EC3-4225-A6F7-4F42F8FF728E",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(45.4500)
        ),
        Row.create(
          "D6D8DDDC-4B0B-4D74-8EDC-A54E9B7F35F7",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(50.0000)
        ),
        Row.create(
          "2C3FC180-D0DF-4D7B-A271-E6CCD2440393",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(50.0000)
        ),
        Row.create(
          "5883CB62-D792-4EE3-ACBC-FE85B6BAA998",
          "D5137D3A-894A-4109-9986-E982541B434F",
          BigDecimal.valueOf(55.0000)
        )
      )

      val result = execute(query).map { case (id, productId, price) =>
        Row(id, productId, price)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("correlated subquery in where clause - return orders where price was above average for particular product") {
      import SqlServerSpecific.SqlServerFunctionDef._
      import DerivedTableSchema._
      import OrderDetails._

      /**
       *  select derived.order_id, derived.product_id, derived.unit_price from order_details derived
       *  where derived.unit_price  > (select avg(order_details.unit_price) from order_details where derived.product_id = order_details.product_id)
       */

      val query = select(derivedOrderId, derivedProductId, derivedUnitPrice)
        .from(orderDetailsDerived)
        .where(
          derivedUnitPrice > subselect[orderDetailsDerived.TableType](Avg(unitPrice))
            .from(orderDetails)
            .where(productId === derivedProductId)
        )

      case class Row(orderId: UUID, productId: UUID, unitPrice: BigDecimal)

      object Row {
        def apply(orderId: String, productId: String, unitPrice: BigDecimal): Row =
          new Row(UUID.fromString(orderId), UUID.fromString(productId), unitPrice.setScale(4, RoundingMode.CEILING))
      }

      val expected = Seq(
        Row("d4e77298-d829-4e36-a6a0-902403f4b7d3", "05182725-f5c8-4fd6-9c43-6671e179bf55", BigDecimal.valueOf(2.0)),
        Row("d6d8dddc-4b0b-4d74-8edc-a54e9b7f35f7", "05182725-f5c8-4fd6-9c43-6671e179bf55", BigDecimal.valueOf(2.0)),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(80.0)),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(80.0)),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(80.0)),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "105a2701-ef93-4e25-81ab-8952cc7d9daa", BigDecimal.valueOf(74.0)),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "4c770002-4c8f-455a-96ff-36a8186d5290", BigDecimal.valueOf(22.0)),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("62cd4109-3e5d-40cc-8188-3899fc1f8bdf", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(10.9)),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(12.0)),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(12.0)),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(12.0)),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(11.0)),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "7368abf4-aed2-421f-b426-1725de756895", BigDecimal.valueOf(12.0)),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "d5137d3a-894a-4109-9986-e982541b434f", BigDecimal.valueOf(55.0)),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "d5137d3a-894a-4109-9986-e982541b434f", BigDecimal.valueOf(55.0)),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0)),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "f35b0053-855b-4145-abe1-dc62bc1fdb96", BigDecimal.valueOf(6.0))
      )

      val x = ""
      val _ = x

      // TODO JDBC is mapped to scala.math big decimal ?
      val result = execute(query).map { case row =>
        Row(row._1, row._2, row._3)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("cross apply - top 1 order_date") {
      import DerivedTableSchema._

      /**
       *  select customers.id, customers.first_name, customers.last_name, derived.order_date
       *  from customers
       *  cross apply (
       *      select top 1 order_date
       *      from orders
       *      where orders.customer_id = customers.id
       *      order by orders.order_date DESC
       *  ) derived
       *  order by derived.order_date desc
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
      import SqlServerSpecific.SqlServerTable._

      val query =
        select(customerId, fName, lName, orderDateDerived)
          .from(customers.crossApply(orderDateDerivedTable))
          .orderBy(Ordering.Desc(orderDateDerived))

      val result = execute(query).map { case row =>
        Row(row._1, row._2, row._3, row._4)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("cross apply with subselect") {
      import OrderSchema._

      /**
       * select customers.first_name, customers.last_name, ooo.order_date
       * from customers
       * cross apply (
       *     select order_date
       *     from orders
       *     where orders.customer_id = customers.id
       * ) ooo
       */
      import SqlServerSpecific.SqlServerTable._
      val subquery =
        subselect[customers.TableType](orderDate, orderId)
          .from(orders)
          .where(customerId === fkCustomerId)
          .asTable("ooo")

      val (orderDateDerived, _) = subquery.columns

      val query = select(fName, lName, orderDateDerived).from(customers.crossApply(subquery))

      val result = execute(query).map { case row =>
        CustomerAndDateRow(row._1, row._2, row._3)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))
    },
    test("cross apply with subquery") {
      import SqlServerSpecific.SqlServerTable._
      import OrderSchema._

      val subquery =
        customers.subselect(orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived = subquery.columns

      val query = select(fName, lName, orderDateDerived).from(customers.crossApply(subquery))

      val result = execute(query).map { case row =>
        CustomerAndDateRow(row._1, row._2, row._3)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))
    },
    test("outer apply") {
      import SqlServerSpecific.SqlServerTable._
      import OrderSchema._

      val subquery =
        subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived = subquery.columns

      val query = select(fName, lName, orderDateDerived).from(customers.outerApply(subquery))

      val result = execute(query).map { case row =>
        CustomerAndDateRow(row._1, row._2, row._3)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))
    },
    test("handle isTrue to 1 bit type") {

      import AggregationDef._

      val query =
        select(Count(customerId))
          .from(customers)
          .where(verified.isTrue)

      for {
        result <- execute(query).runHead.some
      } yield assertTrue(result == 4L)
    },
    test("handle isNotTrue to 0 bit type") {
      import AggregationDef._

      val query =
        select(Count(customerId))
          .from(customers)
          .where(verified.isNotTrue)

      for {
        result <- execute(query).runHead.some
      } yield assertTrue(result == 1L)
    },
    test("Can select from joined tables (inner join)") {
      import OrderSchema._
      val query = select(fName, lName, orderDate).from(customers.join(orders).on(fkCustomerId === customerId))

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

      val result = execute(query).map { case row =>
        Row(row._1, row._2, row._3)
      }

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    test("Can update rows") {
      val query = update(customers).set(fName, "Roland").where(fName === "Ronald")

      assertZIO(execute(query))(equalTo(1))
    },
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers).where(verified.isNotTrue)

      for {
        result <- execute(query)
      } yield assertTrue(result == 1)
    },
    test("Can insert rows") {

      val rows = List(
        CustomerRow(UUID.randomUUID(), "Peter", "Parker", true, LocalDate.ofYearDay(2001, 8)),
        CustomerRow(UUID.randomUUID(), "Stephen", "Strange", false, LocalDate.ofYearDay(1980, 2))
      )

      val command = insertInto(customers)(
        customerId,
        fName,
        lName,
        verified,
        dob
      ).values(rows)

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert tuples") {
      import OrderSchema._

      val rows = List(
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 1)
        ),
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 5)
        )
      )

      val command = insertInto(orders)(
        orderId,
        fkCustomerId,
        orderDate
      ).values(rows)

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert all supported types") {
      import AllTypesHelper._
      import AllTypes._
      import zio.prelude._

      val sqlMinDateTime = LocalDateTime.of(1, 1, 1, 0, 0)
      val sqlMaxDateTime = LocalDateTime.of(9999, 12, 31, 23, 59)

      val maxOffsetSeconds = 14 * 3600
      val sqlInstant       = Gen.instant(
        sqlMinDateTime.toInstant(ZoneOffset.ofTotalSeconds(-maxOffsetSeconds)),
        sqlMaxDateTime.toInstant(ZoneOffset.ofTotalSeconds(maxOffsetSeconds))
      )

      val sqlYear = Gen.int(1, 9999).map(Year.of)

      val sqlLocalDate = for {
        year  <- sqlYear
        month <- Gen.int(1, 12)
        maxLen = if (!year.isLeap && month == 2) 28 else Month.of(month).maxLength
        day   <- Gen.int(1, maxLen)
      } yield LocalDate.of(year.getValue, month, day)

      val sqlLocalDateTime = Gen.localDateTime(sqlMinDateTime, sqlMaxDateTime)

      val zoneOffset = Gen
        .int(-maxOffsetSeconds / 60, maxOffsetSeconds / 60)
        .map(p => ZoneOffset.ofTotalSeconds(p * 60))

      val sqlZonedDateTime = for {
        dateTime <- sqlLocalDateTime
        zOffset  <- zoneOffset
      } yield ZonedDateTime.of(dateTime, ZoneId.from(zOffset))

      val sqlOffsetTime = for {
        localTime <- Gen.localTime
        zOffset   <- zoneOffset
      } yield OffsetTime.of(localTime, zOffset)

      val sqlOffsetDateTime = for {
        dateTime <- sqlLocalDateTime
        zOffset  <- zoneOffset
      } yield OffsetDateTime.of(dateTime, zOffset)

      val javaBigDecimalGen = for {
        bd <- Gen.bigDecimal(Long.MinValue, Long.MaxValue)
      } yield bd.bigDecimal

      val gen = (
        Gen.uuid,
        Gen.chunkOf(Gen.byte),
        javaBigDecimalGen,
        Gen.boolean,
        Gen.char,
        Gen.double,
        Gen.float,
        sqlInstant,
        Gen.int,
        Gen.option(Gen.int),
        sqlLocalDate,
        sqlLocalDateTime,
        Gen.localTime,
        Gen.long,
        sqlOffsetDateTime,
        sqlOffsetTime,
        Gen.short,
        Gen.string,
        Gen.uuid,
        sqlZonedDateTime
      ).tupleN

      case class RowDates(
        localDate: LocalDate,
        localDateTime: LocalDateTime,
        localTime: LocalTime,
        offsetDateTime: Instant,
        offsetTime: OffsetTime,
        zonedDateTime: Instant
      )
      case class RowBasic(
        id: UUID,
        bigDecimal: BigDecimal,
        boolean: Boolean,
        char: Char,
        double: Double,
        float: Float,
        int: Int,
        long: Long,
        short: Short,
        string: String,
        bytearray: Chunk[Byte]
      )

      check(gen) { row =>
        val insert = insertInto(allTypes)(
          id,
          bytearrayCol,
          bigdecimalCol,
          booleanCol,
          charCol,
          doubleCol,
          floatCol,
          instantCol,
          intCol,
          optionalIntCol,
          localdateCol,
          localdatetimeCol,
          localtimeCol,
          longCol,
          offsetdatetimeCol,
          offsettimeCol,
          shortCol,
          stringCol,
          uuidCol,
          zonedDatetimeCol
        ).values(row)

        val query = select(
          id,
          bigdecimalCol,
          booleanCol,
          charCol,
          doubleCol,
          floatCol,
          intCol,
          longCol,
          shortCol,
          stringCol,
          bytearrayCol
        )
          .from(allTypes)
          .where(id === row._1)

        val expectedBasic = RowBasic(
          row._1,
          row._3,
          row._4,
          row._5,
          row._6,
          row._7,
          row._9,
          row._14,
          row._17,
          row._18,
          row._2
        )
        val assertionB    = for {
          _   <- execute(insert)
          rb  <- execute(query).runHead.some
          rowB = RowBasic(rb._1, rb._2, rb._3, rb._4, rb._5, rb._6, rb._7, rb._8, rb._9, rb._10, rb._11)
        } yield assert(rowB) {
          val assertB = assertM(expectedBasic) _
          assertB("UUID")(_.id) &&
          assertB("BigDecimal")(_.bigDecimal) &&
          assertB("Boolean")(_.boolean) &&
          assertB("Char")(_.char) &&
          assertB("Double")(_.double) &&
          assertB("Float")(_.float) &&
          assertB("Int")(_.int) &&
          assertB("Long")(_.long) &&
          assertB("Short")(_.short) &&
          assertB("String")(_.string) &&
          assertB("Chunk[Byte]")(_.bytearray)
        }
        assertionB.mapErrorCause(cause => Cause.stackless(cause.untraced))

        val queryDates =
          select(localdateCol, localdatetimeCol, localtimeCol, offsetdatetimeCol, offsettimeCol, zonedDatetimeCol)
            .from(allTypes)
            .where(id === row._1)

        val expectedDates =
          RowDates(row._11, normLdt(row._12), normLt(row._13), normOdt(row._15), normOt(row._16), normZdt(row._20))
        val assertionD    = for {
          _   <- execute(insert)
          rd  <- execute(queryDates).runHead.some
          rowD = RowDates(rd._1, normLdt(rd._2), normLt(rd._3), normOdt(rd._4), normOt(rd._5), normZdt(rd._6))
        } yield assert(rowD) {
          val assertD = assertM(expectedDates) _
          assertD("LocalTime")(_.localTime) &&
          assertD("LocalDate")(_.localDate) &&
          assertD("LocalDateTime")(_.localDateTime) &&
          //              assertD("OffsetDateTime")(_.offsetDateTime) &&
          //              assertD("OffsetTime")(_.offsetTime) &&
          assertD("zonedDateTime")(_.zonedDateTime)
        }
        assertionD.mapErrorCause(cause => Cause.stackless(cause.untraced))
      }
    } @@ samples(1) @@ retries(0) @@ shrinks(0)
  ) @@ sequential

  private object AllTypesHelper {
    def normLt(in: LocalTime): LocalTime =
      in.truncatedTo(ChronoUnit.MICROS)

    def normLdt(in: LocalDateTime): LocalDateTime =
      LocalDateTime.of(in.toLocalDate, normLt(in.toLocalTime))

    def normOt(in: OffsetTime): OffsetTime =
      OffsetTime.of(normLt(in.toLocalTime), in.getOffset)

    def normOdt(in: OffsetDateTime): Instant =
      OffsetDateTime.of(normLdt(in.toLocalDateTime), in.getOffset).toInstant

    def normZdt(in: ZonedDateTime): Instant =
      ZonedDateTime.of(normLdt(in.toLocalDateTime), in.getZone).toInstant

    def assertM[B, T](expected: B)(name: String)(extract: B => T): Assertion[B] =
      assertionRec[B, T](name)(equalTo(extract(expected)))(p => Option(extract(p)))
  }

  case class CustomerAndDateRow(firstName: String, lastName: String, orderDate: LocalDate)
  private val crossOuterApplyExpected = Seq(
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2019-03-25")),
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2018-06-04")),
    CustomerAndDateRow("Alana", "Murray", LocalDate.parse("2019-08-19")),
    CustomerAndDateRow("Jose", "Wiggins", LocalDate.parse("2019-08-30")),
    CustomerAndDateRow("Jose", "Wiggins", LocalDate.parse("2019-03-07")),
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2020-03-19")),
    CustomerAndDateRow("Alana", "Murray", LocalDate.parse("2020-05-11")),
    CustomerAndDateRow("Alana", "Murray", LocalDate.parse("2019-02-21")),
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2018-05-06")),
    CustomerAndDateRow("Mila", "Paterso", LocalDate.parse("2019-02-11")),
    CustomerAndDateRow("Terrence", "Noel", LocalDate.parse("2019-10-12")),
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2019-01-29")),
    CustomerAndDateRow("Terrence", "Noel", LocalDate.parse("2019-02-10")),
    CustomerAndDateRow("Ronald", "Russell", LocalDate.parse("2019-09-27")),
    CustomerAndDateRow("Alana", "Murray", LocalDate.parse("2018-11-13")),
    CustomerAndDateRow("Jose", "Wiggins", LocalDate.parse("2020-01-15")),
    CustomerAndDateRow("Terrence", "Noel", LocalDate.parse("2018-07-10")),
    CustomerAndDateRow("Mila", "Paterso", LocalDate.parse("2019-08-01")),
    CustomerAndDateRow("Alana", "Murray", LocalDate.parse("2019-12-08")),
    CustomerAndDateRow("Mila", "Paterso", LocalDate.parse("2019-11-04")),
    CustomerAndDateRow("Mila", "Paterso", LocalDate.parse("2018-10-14")),
    CustomerAndDateRow("Terrence", "Noel", LocalDate.parse("2020-04-05")),
    CustomerAndDateRow("Jose", "Wiggins", LocalDate.parse("2019-01-23")),
    CustomerAndDateRow("Terrence", "Noel", LocalDate.parse("2019-05-14")),
    CustomerAndDateRow("Mila", "Paterso", LocalDate.parse("2020-04-30"))
  )

  object CustomerSchema {
    case class Customers(id: UUID, firstName: String, lastName: String, verified: Boolean, dob: LocalDate)

    implicit val customerSchema = DeriveSchema.gen[Customers]

    val customers = defineTable[Customers]

    val (customerId, fName, lName, verified, dob) =
      customers.columns
  }

  object OrderSchema {
    case class Orders(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Orders]

    val orders = defineTable[Orders]

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }

  object ProductSchema {
    case class ProductPrices(productId: UUID, effective: OffsetDateTime, price: BigDecimal)

    implicit val productPricesSchema = DeriveSchema.gen[ProductPrices]

    val productPrices = defineTable[ProductPrices]

    val (fkProductId, effective, price) = productPrices.columns
  }

  object OrderDetails {
    case class OrderDetails(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

    implicit val orderDetailsSchema = DeriveSchema.gen[OrderDetails]

    val orderDetails = defineTable[OrderDetails]

    val (orderDetailsId, productId, quantity, unitPrice) = orderDetails.columns
  }

  object DerivedTableSchema {
    import CustomerSchema._
    import OrderSchema._
    import OrderDetails._

    val orderDetailsDerived = select(orderDetailsId, productId, unitPrice).from(orderDetails).asTable("derived")

    val (derivedOrderId, derivedProductId, derivedUnitPrice) = orderDetailsDerived.columns

    val orderDateDerivedTable = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    val orderDateDerived = orderDateDerivedTable.columns
  }

  object AllTypes {
    case class AllType(
      id: UUID,
      bytearray: Chunk[Byte],
      bigdecimal: java.math.BigDecimal,
      boolean_ : Boolean,
      char_ : Char,
      double_ : Double,
      float: Float,
      instant: Instant,
      int_ : Int,
      optional_int: Option[Int],
      localdate: LocalDate,
      localdatetime: LocalDateTime,
      localtime: LocalTime,
      long_ : Long,
      offsetdatetime: OffsetDateTime,
      offsettime: OffsetTime,
      short: Short,
      string: String,
      uuid: UUID,
      zoneddatetime: ZonedDateTime
    )

    implicit val alTypesSchema = DeriveSchema.gen[AllType]

    val allTypes = defineTableSmart[AllType]

    val (
      id,
      bytearrayCol,
      bigdecimalCol,
      booleanCol,
      charCol,
      doubleCol,
      floatCol,
      instantCol,
      intCol,
      optionalIntCol,
      localdateCol,
      localdatetimeCol,
      localtimeCol,
      longCol,
      offsetdatetimeCol,
      offsettimeCol,
      shortCol,
      stringCol,
      uuidCol,
      zonedDatetimeCol
    ) = allTypes.columns
  }
}
