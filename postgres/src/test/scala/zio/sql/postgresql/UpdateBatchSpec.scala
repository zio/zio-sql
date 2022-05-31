package zio.sql.postgresql

import zio.Cause
import zio.test.Assertion._
import zio.test._

import java.time.{ LocalDate, ZonedDateTime }
import java.util.UUID

object UpdateBatchSpec extends PostgresRunnableSpec with DbSchema {

  import Customers._

  private def update_(c: Customer): Update[customers.TableType] =
    update(customers)
      .set(verified, !c.verified)
      .where(customerId === c.id)

  override def specLayered = suite("Postgres module batch update")(
    test("Can update more than one customer from single table with a condition") {
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
        r <- resultInsert
      } yield assert(r)(equalTo(4))
      insertAssertion.mapErrorCause(cause => Cause.stackless(cause.untraced))

      val selectAll = select(ALL).from(customers)
      val result_   = execute(selectAll.to((Customer.apply _).tupled)).runCollect

      val assertion_ = for {
        x      <- result_
        updated = x.toList.map(update_)
        result <- executeBatchUpdate(updated).map(l => l.reduce((a, b) => a + b))
      } yield assert(result)(equalTo(5))
      assertion_.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )

}
