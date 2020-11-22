package zio.sql.postgresql

import zio.Cause
import zio.test._
import zio.test.Assertion._
import zio.random.Random

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
    testM("sin") {
      val query = select(Sin(1.0)) from customers

      val expected = 0.8414709848078965

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sind") {
      val query = select(Sind(30.0)) from customers

      val expected = 0.5

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("parseIdent removes quoting of individual identifiers") {
      val someString: Gen[Random with Sized, String]    = Gen.anyString
        .filter(x => x.length < 50 && x.length > 1)
      //NOTE: I don't know if property based testing is worth doing here, I just wanted to try it
      val genTestString: Gen[Random with Sized, String] =
        for {
          string1 <- someString
          string2 <- someString
        } yield s"""'"${string1}".${string2}'"""

      val assertion = checkM(genTestString) { (testString) =>
        val query      = select(ParseIdent(testString)) from customers
        val testResult = execute(query).to[String, String](identity)

        for {
          r <- testResult.runCollect
        } yield assert(r.head)(not(containsString("'")) && not(containsString("\"")))

      }
      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("parseIdent fails with invalid identifier") {
      val query      = select(ParseIdent("\'\"SomeSchema\".someTable.\'")) from customers
      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect.run
      } yield assert(r)(fails(anything))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
