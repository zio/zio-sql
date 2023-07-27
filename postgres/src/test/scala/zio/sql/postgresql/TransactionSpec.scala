package zio.sql.postgresql

import zio._
import zio.sql.update.Update
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.sequential

import java.time.{LocalDate, ZonedDateTime}
import java.util.UUID

object TransactionSpec extends PostgresRunnableSpec with DbSchema {

  override val autoCommit = false

  import CustomerSchema._

  private def update_(c: Customer): Update[customers.TableType] =
    update(customers)
      .set(verified, !c.verified)
      .where(customerId === c.id)

  override def specLayered = suite("Postgres module")(
    test("Transaction is returning the last value") {
      val query = select(customerId) from customers

      val result = transact(
        query.run.runCount *> query.run.runCount
      )

      val assertion = assertZIO(result)(equalTo(5L)).orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction failed and didn't delete rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val result = (for {
        allCustomersCount       <- execute(query).runCount
        _                       <- transact {
                                     deleteQuery.run *> ZIO.fail(new Exception("this is error")) *> query.run.runCount
                                   }.catchAllCause(_ => ZIO.unit)
        remainingCustomersCount <- execute(query).runCount
      } yield (allCustomersCount, remainingCustomersCount))

      assertZIO(result)(equalTo((5L, 5L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction succeeded and deleted rows") {
      val deleteQuery = deleteFrom(customers).where(verified === false)
      val id1 = UUID.randomUUID()
      val id2 = UUID.randomUUID()

      val c1 = Customer(
        id1,
        LocalDate.now(),
        "fnameCustomer1",
        "lnameCustomer1",
        true,
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val c2 = Customer(
        id2,
        LocalDate.now(),
        "fnameCustomer2",
        "lnameCustomer2",
        true,
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val allCustomer = List(c1, c2)
      val data = allCustomer.map(Customer.unapply(_).get)
      val insertStmt = insertInto(customers)(ALL).values(data)
      val updateStmt = allCustomer.map(update_)

      val batchResult = for {
        deleted<-deleteQuery.run
        inserted<-insertStmt.run
        updated<-updateStmt.run
      }yield deleted+inserted+updated

      val result = for {
        tx  <- transact(batchResult)
      } yield  tx

      assertZIO(result)(equalTo(5)).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
      test ("Transaction failed and no row was inserted updated or deleted") {
      val deleteQuery = deleteFrom(customers).where(verified === false)
      val id1 = UUID.randomUUID()
      //val id2 = UUID.randomUUID()

      val c1 = Customer(
        id1,
        LocalDate.now(),
        "fnameCustomer1",
        "lnameCustomer1",
        true,
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val c2 = Customer(
        id1,
        LocalDate.now(),
        "fnameCustomer2",
        "lnameCustomer2",
        true,
        LocalDate.now().toString,
        ZonedDateTime.now()
      )
      val allCustomer = List(c1, c2)
      val data = allCustomer.map(Customer.unapply(_).get)
      val insertStmt = insertInto(customers)(ALL).values(data)
      val updateStmt = allCustomer.map(update_)

      val batchResult = for {
        deleted <- deleteQuery.run
        inserted <- insertStmt.run
        updated <- updateStmt.run
      } yield deleted + inserted + updated

      val result = for {
        tx <- transact(batchResult)
      } yield tx
     assertZIO(result)(equalTo(0)).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
