package zio.sql.oracle

import java.math.BigDecimal
import java.util.UUID
import java.time._
import zio.Chunk
import zio.schema.{ DeriveSchema, Schema }
import zio.sql.table._

trait ShopSchema extends OracleSqlModule { self =>

  object Customers {

    case class Customers(id: UUID, dob: LocalDate, first_name: String, last_name: String, verified: Boolean)

    implicit val customerSchema: Schema.CaseClass5[UUID, LocalDate, String, String, Boolean, Customers] =
      DeriveSchema.gen[Customers]

    val customers = Table.defineTableSmart[Customers]

    val (customerId, dob, fName, lName, verified) = customers.columns
  }
  object Orders {
    case class Order(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema: Schema.CaseClass3[UUID, UUID, LocalDate, Order] = DeriveSchema.gen[Order]

    val orders = Table.defineTableSmart[Order]

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }

  object ProductPrices {
    case class ProductPrice(productId: UUID, effective: LocalDate, price: BigDecimal)
    implicit val productPriceSchema: Schema.CaseClass3[UUID, LocalDate, BigDecimal, ProductPrice] =
      DeriveSchema.gen[ProductPrice]

    val productPrices = Table.defineTableSmart[ProductPrice]

    val (productPricesOrderId, effectiveDate, productPrice) = productPrices.columns
  }

  object OrderDetailsSchema {
    case class OrderDetails(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

    implicit val orderDetailsSchema: Schema.CaseClass4[UUID, UUID, Int, BigDecimal, OrderDetails] =
      DeriveSchema.gen[OrderDetails]

    val orderDetails = Table.defineTableSmart[OrderDetails]

    val (orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice) = orderDetails.columns
  }

  object AllTypes {

    case class AllType(
      id: UUID,
      bytearray: Chunk[Byte],
      bigdecimal: BigDecimal,
      boolean_ : Boolean,
      char_ : Char,
      double_ : Double,
      float_ : Float,
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

    implicit val alTypesSchema: Schema.CaseClass20[
      UUID,
      Chunk[Byte],
      BigDecimal,
      Boolean,
      Char,
      Double,
      Float,
      Instant,
      Int,
      Option[Int],
      LocalDate,
      LocalDateTime,
      LocalTime,
      Long,
      OffsetDateTime,
      OffsetTime,
      Short,
      String,
      UUID,
      ZonedDateTime,
      AllType
    ] = DeriveSchema.gen[AllType]

    val allTypes = Table.defineTableSmart[AllType]

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
