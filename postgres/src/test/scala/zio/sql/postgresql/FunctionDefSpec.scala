package zio.sql.postgresql

import zio.Cause
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
    testM("concat_ws #1") {
      import Expr._
//      val idealApi = ConcatWs(" ", "Person:", string("first_name"), string("last_name"), "!")

      //TODO: we shouldn't be forced to provide explicit calls to literal
      val args_0/*: Seq[Expr[DataTypes, Any, String]]*/ = Seq(literal(" "), literal("Person:")) //Seq(literal(" "), literal("Person:"))
//      val args_0/*: Seq[Expr[DataTypes, Any, String]]*/ = Seq(Customers.fName, Customers.lName)
//      val args_0/*: Seq[Expr[Expr.DataSource, Any, String]]*/ = Seq(Customers.fName, literal("!"))

//      val args_0/*: Seq[Expr[DataTypes, Any, String]]*/ = Seq(literal(" "), literal("Person:"), Customers.fName, Customers.lName, literal("!"))

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
