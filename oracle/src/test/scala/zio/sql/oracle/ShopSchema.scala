package zio.sql.oracle

import java.util.UUID
import java.time._
import zio.Chunk
import zio.schema.DeriveSchema

trait ShopSchema extends OracleSqlModule { self =>

  object Customers {

    case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

    implicit val customerSchema = DeriveSchema.gen[Customers]

    val customers = defineTableSmart[Customers]

    val (customerId, dob, fName, lName, verified) = customers.columns
  }
  object Orders {
    case class Order(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Order]

    val orders = defineTableSmart[Order]

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }

  object AllTypes {

    case class AllType(
      id: UUID,
      bytearray: Chunk[Byte],
      bigdecimal: java.math.BigDecimal,
      boolean_ : Boolean,
      char_ : Char,
      double_ : Double,
      float: Float,
      instant: Instant,
      int_ : Int,
      optional_int: Option[Int],
      localdate: LocalDate,
      localdatetime: LocalDateTime,
      localtime: LocalTime,
      long_ : Long,
      offsetdatetime: OffsetDateTime,
      offsettime: OffsetTime,
      short: Short,
      string: String,
      uuid: UUID,
      zoneddatetime: ZonedDateTime
    )

    implicit val alTypesSchema = DeriveSchema.gen[AllType]

    val allTypes = defineTableSmart[AllType]

    val (
      id,
      bytearrayCol,
      bigdecimalCol,
      booleanCol,
      charCol,
      doubleCol,
      floatCol,
      instantCol,
      intCol,
      optionalIntCol,
      localdateCol,
      localdatetimeCol,
      localtimeCol,
      longCol,
      offsetdatetimeCol,
      offsettimeCol,
      shortCol,
      stringCol,
      uuidCol,
      zonedDatetimeCol
    ) = allTypes.columns
  }
}
