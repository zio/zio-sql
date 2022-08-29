package zio.sql.postgresql

import zio._
import zio.test._
import zio.test.TestAspect.sequential
import java.util.UUID
import java.time._
import zio.schema.DeriveSchema

object TransactionSpec extends PostgresRunnableSpec {

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

  override val autoCommit = false

  override def specLayered = suite("Postgres module")(
    test("Transaction is returning the last value") {
      val query = select(customerId) from customers

      for {
        transaction <- execute(ZTransaction(query) *> ZTransaction(query))
        result      <- transaction.runCount
      } yield assertTrue(result == 5L)
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
        allCustomersCount       <- execute(query).runCount
        _                       <- execute(
                                     ZTransaction(deleteQuery) *> ZTransaction.fail(new Exception("this is error")) *> ZTransaction(query)
                                   ).catchAllCause(_ => ZIO.unit)
        remainingCustomersCount <- execute(query).runCount
      } yield assertTrue((allCustomersCount, remainingCustomersCount) == ((5L, 5L)))
    },
    test("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = ZTransaction(deleteQuery)

      for {
        allCustomersCount       <- execute(query).runCount
        _                       <- execute(tx)
        remainingCustomersCount <- execute(query).runCount
      } yield assertTrue((allCustomersCount, remainingCustomersCount) == ((5L, 4L)))
    }
  ) @@ sequential
}
