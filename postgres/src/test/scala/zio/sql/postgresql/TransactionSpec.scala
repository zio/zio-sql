package zio.sql.postgresql

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.sequential

object TransactionSpec extends PostgresRunnableSpec with DbSchema {

  override val autoCommit = false

  import Customers._

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
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = transact(deleteQuery.run)

      val result = (for {
        allCustomersCount       <- execute(query).runCount
        _                       <- tx
        remainingCustomersCount <- execute(query).runCount
      } yield (allCustomersCount, remainingCustomersCount))

      assertZIO(result)(equalTo((5L, 4L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
