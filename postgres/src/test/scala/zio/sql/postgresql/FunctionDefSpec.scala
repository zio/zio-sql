package zio.sql.postgresql

import zio.Cause
import zio.stream.ZStream
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  private def collectAndCompare(
    expected: Seq[String],
    testResult: ZStream[FunctionDefSpec.ReadExecutor, Exception, String]
  ): zio.ZIO[FunctionDefSpec.Environment, Any, TestResult] = {
    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r.toList)(equalTo(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

  val spec = suite("Postgres FunctionDef")(
    testM("concat_ws #1 - combine flat values") {
      import Expr._

      //note: a plain number (3) would and should not compile
      val query = select(ConcatWs4("+", "1", "2", "3")) from customers
      println(renderRead(query))

      val expected = Seq( // note: one for each row
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3"
      )

      val testResult = execute(query).to[String, String](identity)
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #2 - combine columns") {
      import Expr._

      // note: you can't use customerId here as it is a UUID, hence not a string in our book
      val query = select(ConcatWs3(Customers.fName, Customers.fName, Customers.lName)) from customers
      println(renderRead(query))

      val expected = Seq(
        "RonaldRonaldRussell",
        "TerrenceTerrenceNoel",
        "MilaMilaPaterso",
        "AlanaAlanaMurray",
        "JoseJoseWiggins"
      )

      val testResult = execute(query).to[String, String](identity)
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #3 - combine columns and flat values") {
      import Expr._

      val query = select(ConcatWs4(" ", "Person:", Customers.fName, Customers.lName)) from customers
      println(renderRead(query))

      val expected = Seq(
        "Person: Ronald Russell",
        "Person: Terrence Noel",
        "Person: Mila Paterso",
        "Person: Alana Murray",
        "Person: Jose Wiggins"
      )

      val testResult = execute(query).to[String, String](identity)
      collectAndCompare(expected, testResult)
    },
    testM("concat_ws #3 - combine function calls together") {
      import Expr._

      val query = select(
        ConcatWs3(" and ", Concat("Name: ", Customers.fName), Concat("Surname: ", Customers.lName))
      ) from customers
      println(renderRead(query))

      val expected = Seq(
        "Name: Ronald and Surname: Russell",
        "Name: Terrence and Surname: Noel",
        "Name: Mila and Surname: Paterso",
        "Name: Alana and Surname: Murray",
        "Name: Jose and Surname: Wiggins"
      )

      val testResult = execute(query).to[String, String](identity)
      collectAndCompare(expected, testResult)
    },
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
    }
  )
}
