package zio.sql.mysql

import java.util.UUID

import zio._
import zio.test._
import zio.test.TestAspect.sequential
import java.time.LocalDate
import zio.schema.DeriveSchema

object TransactionSpec extends MysqlRunnableSpec {

  case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = defineTable[Customers]

  val (customerId, dob, fName, lName, verified) = customers.columns

  override def specLayered = suite("MySQL module")(
    test("Transaction is returning the last value") {
      val query = select(customerId) from customers

      for {
        result <- execute(ZTransaction(query) *> ZTransaction(query))
        count  <- result.runCount
      } yield assertTrue(count == 5)
    },
    test("Transaction is failing") {
      val query = select(customerId) from customers

      for {
        result <- execute(ZTransaction(query) *> ZTransaction.fail(new Exception("failing")) *> ZTransaction(query))
                    .mapError(_.getMessage)
                    .flip
      } yield assertTrue(result == "failing")
    },
    test("Transaction failed and didn't deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
        _                       <- execute(
                                     ZTransaction(deleteQuery) *> ZTransaction.fail(new Exception("this is error")) *> ZTransaction(query)
                                   ).catchAllCause(_ => ZIO.unit)
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
      } yield assertTrue((allCustomersCount, remainingCustomersCount) == ((5L, 5L)))
    },
    test("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = ZTransaction(deleteQuery)

      for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
        _                       <- execute(tx)
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
      } yield assertTrue((allCustomersCount, remainingCustomersCount) == ((5L, 4L)))
    }
  ) @@ sequential
}
