package zio.sql.sqlserver

import zio._
import zio.test.Assertion._
import zio.test.TestAspect.sequential
import zio.test._

import java.time._
import java.util.UUID
import scala.language.postfixOps

object PostgresModuleSpec extends SqlServerRunnableSpec with DbSchema {

  import AggregationDef._
  import DbSchema._

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

  override def specLayered = suite("MSSQL Server module")(
    test("Can select from single table") {
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
    test("Can select from single table with limit, offset and order by") {
      case class Customer(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

      val query = select(customerId ++ fName ++ lName ++ dob).from(customers).limit(1).offset(1).orderBy(fName)

      val expected =
        Seq(
          Customer(
            UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"),
            "Alana",
            "Murray",
            LocalDate.parse("1995-11-12")
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
    test("Can count rows") {
      val query = select(Count(customerId)).from(customers)

      val expected = 5L

      val result = execute(query.to[Long, Long](identity))

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    test("correlated subqueries in selections - counts orders for each customer") {

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
    test("subquery in where clause") {
      import SqlServerSpecific.SqlServerFunctionDef._

      /**
       * select order_id, product_id, unit_price from order_details
       * where unit_price > (select AVG(price)
       *                                       from product_prices )
       */

      val query = select(orderDetailsId ++ productId ++ unitPrice)
        .from(orderDetails)
        .where(
          unitPrice > select(Avg(price)).from(productPrices)
        )

      case class Row(orderId: UUID, productId: UUID, unitPrice: BigDecimal)

      object Row {
        def apply(orderId: String, productId: String, unitPrice: BigDecimal): Row =
          new Row(UUID.fromString(orderId), UUID.fromString(productId), unitPrice)
      }

      val expected = Seq(
        Row("04912093-CC2E-46AC-B64C-1BD7BB7758C3", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("9022DD0D-06D6-4A43-9121-2993FC7712A1", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("38D66D44-3CFA-488A-AC77-30277751418F", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("7B2627D5-0150-44DF-9171-3462E20797EE", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("62CD4109-3E5D-40CC-8188-3899FC1F8BDF", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 72.7200),
        Row("9473A0BC-396A-4936-96B0-3EEA922AF36B", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 80.0000),
        Row("B8BAC18D-769F-48ED-809D-4B6C0E4D1795", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 67.2700),
        Row("BEBBFE4D-4EC3-4389-BDC2-50E9EAC2B15B", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 67.2700),
        Row("742D45A0-E81A-41CE-95AD-55B4CABBA258", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 67.2700),
        Row("618AA21F-700B-4CA7-933C-67066CF4CD97", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("606DA090-DD33-4A77-8746-6ED0E8443AB2", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 67.2700),
        Row("FD0FA8D4-E1A0-4369-BE07-945450DB5D36", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 80.0000),
        Row("876B6034-B33C-4497-81EE-B4E8742164C2", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("91CAA28A-A5FE-40D7-979C-BD6A128D0418", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("2C3FC180-D0DF-4D7B-A271-E6CCD2440393", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 70.0000),
        Row("763A7C39-833F-4EE8-9939-E80DFDBFC0FC", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 80.0000),
        Row("5011D206-8EFF-42C4-868E-F1A625E1F186", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("0A48FFB0-EC61-4147-AF56-FC4DBCA8DE0A", "105A2701-EF93-4E25-81AB-8952CC7D9DAA", 74.0000),
        Row("A243FA42-817A-44EC-8B67-22193D212D82", "D5137D3A-894A-4109-9986-E982541B434F", 45.4500),
        Row("62CD4109-3E5D-40CC-8188-3899FC1F8BDF", "D5137D3A-894A-4109-9986-E982541B434F", 50.0000),
        Row("9473A0BC-396A-4936-96B0-3EEA922AF36B", "D5137D3A-894A-4109-9986-E982541B434F", 55.0000),
        Row("852E2DC9-4EC3-4225-A6F7-4F42F8FF728E", "D5137D3A-894A-4109-9986-E982541B434F", 45.4500),
        Row("D6D8DDDC-4B0B-4D74-8EDC-A54E9B7F35F7", "D5137D3A-894A-4109-9986-E982541B434F", 50.0000),
        Row("2C3FC180-D0DF-4D7B-A271-E6CCD2440393", "D5137D3A-894A-4109-9986-E982541B434F", 50.0000),
        Row("5883CB62-D792-4EE3-ACBC-FE85B6BAA998", "D5137D3A-894A-4109-9986-E982541B434F", 55.0000)
      )

      val result = execute(query.to[UUID, UUID, BigDecimal, Row](Row.apply))

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("correlated subquery in where clause - return orders where price was above average for particular product") {
      import SqlServerSpecific.SqlServerFunctionDef._

      /**
       *  select derived.order_id, derived.product_id, derived.unit_price from order_details derived
       *  where derived.unit_price  > (select avg(order_details.unit_price) from order_details where derived.product_id = order_details.product_id)
       */

      val query = select(derivedOrderId ++ derivedProductId ++ derivedUnitPrice)
        .from(orderDetailsDerived)
        .where(
          derivedUnitPrice > subselect[orderDetailsDerived.TableType](Avg(unitPrice))
            .from(orderDetails)
            .where(productId === derivedProductId)
        )

      case class Row(orderId: UUID, productId: UUID, unitPrice: BigDecimal)

      object Row {
        def apply(orderId: String, productId: String, unitPrice: BigDecimal): Row =
          new Row(UUID.fromString(orderId), UUID.fromString(productId), unitPrice)
      }

      val expected = Seq(
        Row("d4e77298-d829-4e36-a6a0-902403f4b7d3", "05182725-f5c8-4fd6-9c43-6671e179bf55", 2.0),
        Row("d6d8dddc-4b0b-4d74-8edc-a54e9b7f35f7", "05182725-f5c8-4fd6-9c43-6671e179bf55", 2.0),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 80.0),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 80.0),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 80.0),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "105a2701-ef93-4e25-81ab-8952cc7d9daa", 74.0),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "4c770002-4c8f-455a-96ff-36a8186d5290", 22.0),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("62cd4109-3e5d-40cc-8188-3899fc1f8bdf", "7368abf4-aed2-421f-b426-1725de756895", 10.9),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "7368abf4-aed2-421f-b426-1725de756895", 12.0),
        Row("618aa21f-700b-4ca7-933c-67066cf4cd97", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("fd0fa8d4-e1a0-4369-be07-945450db5d36", "7368abf4-aed2-421f-b426-1725de756895", 12.0),
        Row("876b6034-b33c-4497-81ee-b4e8742164c2", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("763a7c39-833f-4ee8-9939-e80dfdbfc0fc", "7368abf4-aed2-421f-b426-1725de756895", 12.0),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "7368abf4-aed2-421f-b426-1725de756895", 11.0),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "7368abf4-aed2-421f-b426-1725de756895", 12.0),
        Row("9473a0bc-396a-4936-96b0-3eea922af36b", "d5137d3a-894a-4109-9986-e982541b434f", 55.0),
        Row("5883cb62-d792-4ee3-acbc-fe85b6baa998", "d5137d3a-894a-4109-9986-e982541b434f", 55.0),
        Row("04912093-cc2e-46ac-b64c-1bd7bb7758c3", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("9022dd0d-06d6-4a43-9121-2993fc7712a1", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("38d66d44-3cfa-488a-ac77-30277751418f", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("7b2627d5-0150-44df-9171-3462e20797ee", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("91caa28a-a5fe-40d7-979c-bd6a128d0418", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("5011d206-8eff-42c4-868e-f1a625e1f186", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0),
        Row("0a48ffb0-ec61-4147-af56-fc4dbca8de0a", "f35b0053-855b-4145-abe1-dc62bc1fdb96", 6.0)
      )

      val result = execute(
        query
          .to[UUID, UUID, BigDecimal, Row] { case row =>
            Row(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("cross apply - top 1 order_date") {

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
        select(customerId ++ fName ++ lName ++ orderDateDerived)
          .from(customers.crossApply(orderDateDerivedTable))
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
    test("cross apply with subselect") {

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
        subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived :*: _ = subquery.columns

      val query = select(fName ++ lName ++ orderDateDerived).from(customers.crossApply(subquery))

      val result = execute(
        query
          .to[String, String, LocalDate, CustomerAndDateRow] { case row =>
            CustomerAndDateRow(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("cross apply with subquery") {
      import SqlServerSpecific.SqlServerTable._
      val subquery =
        customers.subselect(orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived :*: _ = subquery.columns

      val query = select(fName ++ lName ++ orderDateDerived).from(customers.crossApply(subquery))

      val result = execute(
        query
          .to[String, String, LocalDate, CustomerAndDateRow] { case row =>
            CustomerAndDateRow(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("cross apply with subselect from") {
      import SqlServerSpecific.SqlServerTable._
      val subquery =
        subselectFrom(customers)(orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived :*: _ = subquery.columns

      val query = select(fName ++ lName ++ orderDateDerived).from(customers.crossApply(subquery))

      val result = execute(
        query
          .to[String, String, LocalDate, CustomerAndDateRow] { case row =>
            CustomerAndDateRow(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("outer apply") {
      import SqlServerSpecific.SqlServerTable._
      val subquery =
        subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")

      val orderDateDerived :*: _ = subquery.columns

      val query = select(fName ++ lName ++ orderDateDerived).from(customers.outerApply(subquery))

      val result = execute(
        query
          .to[String, String, LocalDate, CustomerAndDateRow] { case row =>
            CustomerAndDateRow(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(crossOuterApplyExpected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Can select from joined tables (inner join)") {
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
    }
  ) @@ sequential

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
}
