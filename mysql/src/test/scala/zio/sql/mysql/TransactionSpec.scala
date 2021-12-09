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

      val result = execute(
        ZTransaction(query) *> ZTransaction(query)
      ).use(ZIO.succeed(_))

      val assertion =
        result
          .flatMap(_.runCount)
          .map(count => assertTrue(count == 5))
          .orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction is failing") {
      val query = select(customerId) from customers

      val result = execute(
        ZTransaction(query) *> ZTransaction.fail(new Exception("failing")) *> ZTransaction(query)
      ).mapError(_.getMessage).use(ZIO.succeed(_))

      assertM(result.flip)(equalTo("failing")).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction failed and didn't deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val result = (for {
        allCustomersCount       <- execute(query.to(identity[UUID](_))).runCount.toManaged
        _                       <- execute(
                                     ZTransaction(deleteQuery) *> ZTransaction.fail(new Exception("this is error")) *> ZTransaction(query)
                                   ).catchAllCause(_ => ZManaged.succeed("continue"))
        remainingCustomersCount <- execute(query.to(identity[UUID](_))).runCount.toManaged
      } yield (allCustomersCount, remainingCustomersCount)).use(ZIO.succeed(_))

      assertM(result)(equalTo((5L, 5L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    test("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = ZTransaction(deleteQuery)

      val result = (for {
        allCustomersCount       <- execute(query.to(identity[UUID](_))).runCount.toManaged
        _                       <- execute(tx)
        remainingCustomersCount <- execute(query.to(identity[UUID](_))).runCount.toManaged
      } yield (allCustomersCount, remainingCustomersCount)).use(ZIO.succeed(_))

      assertM(result)(equalTo((5L, 4L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
