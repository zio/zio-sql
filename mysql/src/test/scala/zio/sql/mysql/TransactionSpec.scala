package zio.sql.mysql

import java.util.UUID

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.sequential

object TransactionSpec extends MysqlRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered = suite("MySQL module")(
    testM("Transaction is returning the last value") {
      val query = select(customerId) from customers

      val result = execute(
        ZTransaction(query) *> ZTransaction(query)
      ).use(ZIO.succeed(_))

      val assertion = assertM(result.flatMap(_.runCount))(equalTo(5L)).orDie

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Transaction is failing") {
      val query = select(customerId) from customers

      val result = execute(
        ZTransaction(query) *> ZTransaction.fail(new Exception("failing")) *> ZTransaction(query)
      ).mapError(_.getMessage).use(ZIO.succeed(_))

      assertM(result.flip)(equalTo("failing")).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Transaction failed and didn't deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val result = (for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount.toManaged_
        _                       <- execute(
                                     ZTransaction(deleteQuery) *> ZTransaction.fail(new Exception("this is error")) *> ZTransaction(query)
                                   ).catchAllCause(_ => ZManaged.succeed("continue"))
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount.toManaged_
      } yield (allCustomersCount, remainingCustomersCount)).use(ZIO.succeed(_))

      assertM(result)(equalTo((5L, 5L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("Transaction succeeded and deleted rows") {
      val query       = select(customerId) from customers
      val deleteQuery = deleteFrom(customers).where(verified === false)

      val tx = ZTransaction(deleteQuery)

      val result = (for {
        allCustomersCount       <- execute(query).map(identity[UUID](_)).runCount.toManaged_
        _                       <- execute(tx)
        remainingCustomersCount <- execute(query).map(identity[UUID](_)).runCount.toManaged_
      } yield (allCustomersCount, remainingCustomersCount)).use(ZIO.succeed(_))

      assertM(result)(equalTo((5L, 4L))).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  ) @@ sequential
}
