package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps

object OracleSqlModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._

  override def specLayered: Spec[SqlDriver, Exception] = suite("Oracle module")(
    test("Can update selected rows") {

      /**
       * UPDATE customers SET customers.first_name = 'Antek'
       * WHERE 1 = 1 and customers.verified = 0 and customers.verified <> 1
       */
      val query =
        update(customers)
          .set(fName, "Antek")
          .where(verified isNotTrue)
          .where(verified <> true) // we intentionally verify two syntax variants

      assertZIO(execute(query))(equalTo(1))
    },
    test("Can update all rows") {

      /**
       * UPDATE customers SET customers.first_name = 'Antek' WHERE 1 = 1
       */
      val query = update(customers).set(fName, "Antek")

      assertZIO(execute(query))(equalTo(5))
    },
    test("Can delete from single table with a condition") {

      /**
       * DELETE FROM customers WHERE customers.verified = 0
       */
      val query = deleteFrom(customers) where (verified isNotTrue)

      val expected = 1
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    },
    test("Can delete all from a single table") {

      /**
       * DELETE FROM customers
       */
      val query = deleteFrom(customers)

      val expected = 4
      val result   = execute(query)

      assertZIO(result)(equalTo(expected))
    }
  ) @@ sequential
}
