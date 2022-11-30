package zio.sql.sqlserver

import java.util.UUID
import java.time._
import java.math.BigDecimal
import zio.schema.DeriveSchema

trait DbSchema extends SqlServerSqlModule { self =>

  object DbSchema {

    case class Customer(
      id: UUID,
      dob: LocalDate,
      firstName: String,
      lastName: String,
      verified: Boolean,
      createdTimestampString: String,
      createdTimestamp: ZonedDateTime
    )

    implicit val custommerSchema = DeriveSchema.gen[Customer]

    val customers = defineTableSmart[Customer]

    val (customerId, dob, fName, lName, verified, createdString, createdTimestamp) =
      customers.columns

    val ALL = customerId ++ dob ++ fName ++ lName ++ verified ++ createdString ++ createdTimestamp

    case class Orders(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Orders]

    val orders = defineTableSmart[Orders]

    val (orderId, fkCustomerId, orderDate) = orders.columns

    case class Products(id: UUID, name: String, description: String, imageUrl: String)

    implicit val productSchema = DeriveSchema.gen[Products]

    val products = defineTableSmart[Products]

    val (productId, productName, description, imageURL) = products.columns

    case class OrderDetails(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

    implicit val orderDetailsSchema = DeriveSchema.gen[OrderDetails]

    val orderDetails = defineTableSmart[OrderDetails]

    val (orderDetailsId, orderDetailsProductId, quantity, unitPrice) = orderDetails.columns

    val orderDetailsDerived =
      select(orderDetailsId, orderDetailsProductId, unitPrice).from(orderDetails).asTable("derived")

    val (derivedOrderId, derivedProductId, derivedUnitPrice) = orderDetailsDerived.columns

    val orderDateDerivedTable = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    val orderDateDerived = orderDateDerivedTable.columns
  }
}
