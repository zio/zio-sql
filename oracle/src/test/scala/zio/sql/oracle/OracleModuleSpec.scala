package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps

object OracleModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered = suite("Postgres module")(
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers) where (verified isNotTrue)
      println(renderDelete(query))

      val expected = 1
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    },
    test("Can delete all from a single table") {
      val query = deleteFrom(customers)
      println(renderDelete(query))

      val expected = 4
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    }
  ) @@ sequential
}
