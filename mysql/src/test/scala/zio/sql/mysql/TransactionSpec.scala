package zio.sql.mysql

import java.util.UUID

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.sequential

object TransactionSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered: Spec[JdbcEnvironment, TestFailure[Object], TestSuccess] = suite("MySQL module")(
    test("Transaction is returning the last value") {
      val query = select(customerId) from customers

      val result = ZIO.scoped {
        execute(
          ZTransaction(query) *> ZTransaction(query)
        )
      }

      val assertion =
        result
          .flatMap(_.runCount)
          .map(count => assertTrue(count == 5))
          .orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction is failing") {
      val query = select(customerId) from customers

      val result = ZIO.scoped {
        execute(
          ZTransaction(query) *> ZTransaction.fail(new Exception("failing")) *> ZTransaction(query)
        ).mapError(_.getMessage)
      }

      assertM(result.flip)(equalTo("failing")).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction failed and didn't deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val result = ZIO.scoped {
        for {
          allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
          _                       <- execute(
                                       ZTransaction(deleteQuery) *> ZTransaction.fail(new Exception("this is error")) *> ZTransaction(query)
                                     ).catchAllCause(_ => ZIO.succeed("continue"))
          remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
        } yield (allCustomersCount, remainingCustomersCount)
      }

      assertM(result)(equalTo((5L, 5L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = ZTransaction(deleteQuery)

      val result = ZIO.scoped {
        for {
          allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount
          _                       <- execute(tx)
          remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount
        } yield (allCustomersCount, remainingCustomersCount)
      }

      assertM(result)(equalTo((5L, 4L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
