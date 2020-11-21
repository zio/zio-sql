package zio.sql.postgresql

import zio.Cause
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._
  import this.ColumnSet._

  val spec = suite("Postgres FunctionDef")(
    testM("concat_ws #1") {
      import FlatValueOrColumnValue._
//      val idealApi = ConcatWs(" ", "Person:", string("first_name"), string("last_name"), "!")

//      val foo: Seq[FlatValueOrColumnValue] = string("first_name") :: string("last_name") :: "!" :: Nil
//      val args: (String, Seq[String]) = (" ", "first_name" :: "last_name" :: "!" :: Nil)
//      val foo: Expr[_, _, _] = "first_name"

//      val args: Seq[Expr[_,_,_]] = "' '" :: "'Person:'" :: "first_name" :: "last_name" :: Nil

//      val args: Expr[_,_,(String, String)] = ("' '", "'Person:', first_name, last_name")

//      val args_2 = " " :: "Person: " :: string("first_name") :: string("last_name") :: "!" :: Nil
//      val args_1: List[FlatValueOrColumnValue] = args_2
//      val args_0: Expr[_, _, String] = args_1

      val args_0: List[FlatValueOrColumnValue] = stringToStringValue(" ") :: stringToStringValue("Person:") :: columnToColumnValue(string("first_name")) :: columnToColumnValue(string("last_name")) :: stringToStringValue("!") :: Nil

      val query = select(ConcatWs(args_0)) from customers
      println(renderRead(query))

      val expected = Seq(
        "Person: Ronald Russell !",
        "Person: Terrence Noel !",
        "Person: Mila Paterso !",
        "Person: Alana Murray !",
        "Person: Jose Wiggins !"
      )

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.toList)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
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
