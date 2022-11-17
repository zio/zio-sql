package zio.sql.mysql

import java.util.UUID

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.sequential

object TransactionSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered = suite("MySQL module")(
    test("Transaction is returning the last value") {
      val query = select(customerId) from customers

      val result = transact(
        query.run.runCount *> query.run.runCount
      )

      val assertion =
        result
          .map(count => assertTrue(count == 5))
          .orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
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

      val result = (for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
        _                       <- transact(
                                     deleteQuery.run *> ZIO.fail(new Exception("this is error")) *> query.run.runCount
                                   ).catchAllCause(_ => ZIO.unit)
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
      } yield (allCustomersCount, remainingCustomersCount))

      assertZIO(result)(equalTo((5L, 5L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = deleteQuery.run

      val result = (for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
        _                       <- transact(tx)
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
      } yield (allCustomersCount, remainingCustomersCount))

      assertZIO(result)(equalTo((5L, 4L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
