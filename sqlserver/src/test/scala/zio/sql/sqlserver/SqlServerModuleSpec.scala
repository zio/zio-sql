package zio.sql.sqlserver

import zio._
import zio.test.Assertion._
import zio.test.TestAspect.{ ignore, sequential }
import zio.test._

import java.time._
import java.util.UUID
import scala.language.postfixOps

object PostgresModuleSpec extends SqlServerRunnableSpec with DbSchema {

  import AggregationDef._
  import Customers._
  import Orders._
  import OrderDetails._

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
    testM("Can select from single table with limit, offset and order by") {
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
    testM("Can count rows") {
      val query = select(Count(customerId)).from(customers)

      val expected = 5L

      val result = execute(query.to[Long, Long](identity))

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    testM("correlated subquery in where clause") {

      /**
       *  select derived.order_id, derived.product_id, derived.unit_price from order_details derived
       *  where derived.unit_price  > (select avg(order_details.unit_price) from order_details where derived.product_id = order_details.product_id)
       */

      val query = select(derivedOrderId ++ derivedProductId ++ derivedUnitPrice)
        .from(orderDetailsDerived)
        .where(
          derivedUnitPrice > subselect[orderDetailsDerived.TableType](AggregationDef.Avg(unitPrice))
            .from(orderDetails)
            .where(productId === derivedProductId)
        )

      case class Row(orderId: UUID, productId: UUID, unitPrice: Double)

      object Row {
        def apply(orderId: String, productId: String, unitPrice: Double): Row =
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
          .to[UUID, UUID, Double, Row] { case row =>
            Row(row._1, row._2, row._3)
          }
      )

      val assertion = for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("cross apply 1") {

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
    testM("outer apply") {
      ???
    } @@ ignore,
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
    }
  ) @@ sequential
}
