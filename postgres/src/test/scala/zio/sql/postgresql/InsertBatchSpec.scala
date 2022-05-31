package zio.sql.postgresql

import zio.Cause
import zio.test.Assertion._
import zio.test._

import java.time.{ LocalDate, ZonedDateTime }
import java.util.UUID

object InsertBatchSpec extends PostgresRunnableSpec with DbSchema {

  import Customers._

  override def specLayered = suite("Postgres module batch insert")(
    test("Can insert more than one customer into a table with a condition") {
      val id1 = UUID.randomUUID()
      val id2 = UUID.randomUUID()
      val id3 = UUID.randomUUID()
      val id4 = UUID.randomUUID()
      val c1  = Customer(
        id1,
        "fnameCustomer1",
        "lnameCustomer1",
        true,
        LocalDate.now(),
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val c2  = Customer(
        id2,
        "fnameCustomer2",
        "lnameCustomer2",
        true,
        LocalDate.now(),
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val c3  = Customer(
        id3,
        "fnameCustomer3",
        "lnameCustomer3",
        true,
        LocalDate.now(),
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val c4  = Customer(
        id4,
        "fnameCustomer4",
        "lnameCustomer4",
        false,
        LocalDate.now(),
        LocalDate.now().toString,
        ZonedDateTime.now()
      )

      val allCustomer = List(c1, c2, c3, c4)
      val data        = allCustomer.map(Customer.unapply(_).get)
      val query       = insertInto(customers)(ALL).values(data)

      val resultInsert = execute(query)

      val insertAssertion = for {
        result <- resultInsert
      } yield assert(result)(equalTo(4))
      insertAssertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )

}
