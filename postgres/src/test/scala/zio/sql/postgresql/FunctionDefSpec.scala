package zio.sql.postgresql

import zio.Cause
import zio.sql.postgresql.ShopSchema
import zio.sql.postgresql.PostgresModule
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec
    extends DefaultRunnableSpec
    with PostgresIntegrationTestBase
    with PostgresModule
    with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
    testM("sin") {
      val query = select { Sin(30.0) } from customers

      val expected = 0.5

      val testResult = new ExecuteBuilder(query)
        .to[Double, Double] { case row =>
          row
        }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.provideCustomLayer(executorLayer)//.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sind") {
      val query = select { Sind(30.0) } from customers

      val expected = 0.5

      val testResult = new ExecuteBuilder(query)
        .to[Double, Double] { case row =>
          row
        }

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.provideCustomLayer(executorLayer).mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}