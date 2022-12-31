package zio.sql.postgresql

import java.time.{ LocalDate, ZonedDateTime }
import java.util.UUID
import zio.schema.DeriveSchema
import java.math.BigDecimal

trait DbSchema extends PostgresJdbcModule { self =>

  object Cities {
    case class City(id: Int, name: String, population: Int, area: Float, link: Option[String])
    case class MetroSystem(id: Int, cityId: Int, name: String, dailyRidership: Int)
    case class MetroLine(id: Int, systemId: Int, name: String, stationCount: Int, trackType: Int)

    implicit val citySchema = DeriveSchema.gen[City]

    val city                                       = defineTable[City]
    val (cityId, cityName, population, area, link) = city.columns

    implicit val metroSystemSchema = DeriveSchema.gen[MetroSystem]

    val metroSystem = defineTable[MetroSystem]

    val (metroSystemId, cityIdFk, metroSystemName, dailyRidership) = metroSystem.columns

    implicit val metroLineSchema = DeriveSchema.gen[MetroLine]

    val metroLine = defineTable[MetroLine]

    val (metroLineId, systemId, metroLineName, stationCount, trackType) = metroLine.columns
  }

  object DerivedTables {
    import OrdersSchema._
    import CustomerSchema._
    import OrderDetailsSchema._

    val orderDetailsDerived =
      select(orderDetailsOrderId, orderDetailsProductId, unitPrice).from(orderDetails).asTable("derived")

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

  object CustomerSchema {
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
  }

  object OrdersSchema {
    case class Orders(id: UUID, customerId: UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Orders]

    val orders = defineTableSmart[Orders]

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }

  object ProductSchema {
    case class Products(id: UUID, name: String, description: String, imageUrl: String)

    implicit val productSchema = DeriveSchema.gen[Products]

    val products = defineTableSmart[Products]

    val (productId, productName, description, imageURL) = products.columns
  }

  object ProductPrices {
    case class ProductPrice(productId: UUID, effective: LocalDate, price: BigDecimal)
    implicit val productPriceSchema = DeriveSchema.gen[ProductPrice]

    val productPrices = defineTableSmart[ProductPrice]

    val (productPricesOrderId, effectiveDate, productPrice) = productPrices.columns
  }

  object OrderDetailsSchema {
    case class OrderDetails(orderId: UUID, productId: UUID, quantity: Int, unitPrice: BigDecimal)

    implicit val orderDetailsSchema = DeriveSchema.gen[OrderDetails]

    val orderDetails = defineTableSmart[OrderDetails]

    val (orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice) = orderDetails.columns
  }

  object PersonsSchema {
    case class Persons(id: UUID, name: Option[String], birthDate: Option[LocalDate])

    implicit val personsSchema = DeriveSchema.gen[Persons]

    val persons = defineTableSmart[Persons]

    val (personsId, personsName, birthDate) = persons.columns
  }
}
