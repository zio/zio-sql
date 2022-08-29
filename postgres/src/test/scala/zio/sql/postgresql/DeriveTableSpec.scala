package zio.sql.postgresql

import zio.test._
import zio.test.Assertion._
import zio.schema._
import java.time.LocalDate
import zio.test.TestAspect._
import java.util.UUID
import java.time._

//TODO remove
object DeriveTableSpec extends PostgresRunnableSpec {

    //====

    case class Customers(
      id: UUID,
      dob: LocalDate,
      firstName: String,
      lastName: String,
      verified: Boolean,
      createdTimestampString: String,
      createdTimestamp: ZonedDateTime
    )

    implicit val custommerSchema = DeriveSchema.gen[Customers]

    val customers = defineTable[Customers]

    val (customerId, dob, fName, lName, verified, createdString, createdTimestamp) =
      customers.columns

    //===  

    case class Orders(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Orders]

    val orders = defineTable[Orders]

    // orderDate: Expr[Features.Source[orderSchema.field3.label.type,Orders],Orders,LocalDate]
    val (orderId, fkCustomerId, orderDate) = orders.columns


  object DerivedTables {

    val orderDateDerivedTable = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    implicitly[orderDateDerivedTable.TableType =:= Customers with Orders]  

    //  
    val orderDateDerived = orderDateDerivedTable.columns
  }

  def specLayered =
    suite("testing new table derivation")(
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
      test("join table") {
        val query = select(fName, lName, orderDate).from(customers.join(orders).on(fkCustomerId === customerId))

        case class ResultRow(firstName: String, lastName: String, orderDate: LocalDate)

        val results = execute(query)
          .map(ResultRow.tupled)
          .runCollect
          .map(_.toList)

        assertZIO(results)(hasSameElementsDistinct(expected.map(ResultRow.tupled)))
      } @@ ignore
    )

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
}
