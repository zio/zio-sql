package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.language.postfixOps
import java.util.UUID
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import zio.schema._

object OracleModuleSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._
  import Orders._

  override def specLayered = suite("Oracle module")(
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
    },
    test("Can insert rows") {
      final case class CustomerRow(
        id: UUID,
        dateOfBirth: LocalDate,
        firstName: String,
        lastName: String,
        verified: Boolean
      )
      implicit val customerRowSchema =
        Schema.CaseClass5[UUID, LocalDate, String, String, Boolean, CustomerRow](
          Schema.Field("id", Schema.primitive[UUID](zio.schema.StandardType.UUIDType)),
          Schema.Field(
            "dateOfBirth",
            Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))
          ),
          Schema.Field("firstName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("lastName", Schema.primitive[String](zio.schema.StandardType.StringType)),
          Schema.Field("verified", Schema.primitive[Boolean](zio.schema.StandardType.BoolType)),
          CustomerRow.apply,
          _.id,
          _.dateOfBirth,
          _.firstName,
          _.lastName,
          _.verified
        )

      val rows = List(
        CustomerRow(UUID.randomUUID(), LocalDate.ofYearDay(2001, 8), "Peter", "Parker", true),
        CustomerRow(UUID.randomUUID(), LocalDate.ofYearDay(1980, 2), "Stephen", "Strange", false)
      )

      val command = insertInto(customers)(
        customerId,
        dob,
        fName,
        lName,
        verified
      ).values(rows)

      println(renderInsert(command))

      assertZIO(execute(command))(equalTo(2))
    },
    test("Can insert tuples") {

      val rows = List(
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 1)
        ),
        (
          UUID.randomUUID(),
          UUID.randomUUID(),
          LocalDate.of(2022, 1, 5)
        )
      )

      val command = insertInto(orders)(
        orderId,
        fkCustomerId,
        orderDate
      ).values(rows)

      println(renderInsert(command))

      assertZIO(execute(command))(equalTo(2))
    }
  ) @@ sequential
}
