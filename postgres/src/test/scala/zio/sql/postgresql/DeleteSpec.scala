package zio.sql.postgresql

import zio.test._
import java.util.UUID
import java.time.LocalDate
import java.time.ZonedDateTime
import zio.schema.DeriveSchema
import zio.sql.table._

object DeleteSpec extends PostgresRunnableSpec {

  case class Customers(
    id: UUID,
    dob: LocalDate,
    firstName: String,
    lastName: String,
    verified: Boolean,
    createdTimestampString: String,
    createdTimestamp: ZonedDateTime
  )

  implicit val custommerSchema = DeriveSchema.gen[Customers]

  val customers = Table.defineTable[Customers]

  val (customerId, dob, fName, lName, verified, createdString, createdTimestamp) =
    customers.columns

  override def specLayered = suite("Postgres module delete")(
    test("Can delete from single table with a condition") {
      val query = deleteFrom(customers).where(verified.isNotTrue)

      val result = execute(query)

      for {
        r <- result
      } yield assertTrue(r == 1)
    }
  )
}
