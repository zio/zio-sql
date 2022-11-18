package zio.sql.mysql

import zio.test._

import java.util.UUID
import java.time.LocalDate
import zio.schema.DeriveSchema
import zio.test.TestAspect.sequential

object DeleteSpec extends MysqlRunnableSpec {

  case class Customers(id: UUID, dob: LocalDate, first_name: String, lastName: String, verified: Boolean)

  implicit val customerSchema = DeriveSchema.gen[Customers]

  val customers = defineTable[Customers]

  val (_, _, _, lastName, verified) = customers.columns

  override def specLayered = suite("MySQL module delete")(
    test("Can delete from single table with a is not true condition") {
      val query = deleteFrom(customers).where(verified.isNotTrue)

      for {
        r <- execute(query)
      } yield assertTrue(r == 1)
    },
    test("Can delete from single table with an equals condition") {
      val query = deleteFrom(customers).where(lastName === "Murray")

      for {
        r <- execute(query)
      } yield assertTrue(r == 1)
    }
  ) @@ sequential
}
