package zio.sql.postgresql

import zio.sql.Jdbc

import java.time.{ LocalDate, ZonedDateTime }
import java.util.UUID

trait DbSchema extends Jdbc { self =>
  import self.ColumnSet._

  object Persons {

    import ColumnSetAspect._

    val persons =
      (uuid("id") ++ string("first_name") ++ string("last_name") ++ (localDate("dob") @@ nullable))
        .table("persons")

    val (personId, fName, lName, dob) = persons.columns
  }

  object Customers     {
    case class Customer(
      id: UUID,
      fname: String,
      lname: String,
      verified: Boolean,
      dateOfBirth: LocalDate,
      created: String,
      created2: ZonedDateTime
    )

    // https://github.com/zio/zio-sql/issues/320 Once Insert is supported, we can remove created_timestamp_string
    val customers =
      (uuid("Id") ++ localDate("Dob") ++ string("First_name") ++ string("Last_name") ++ boolean(
        "Verified"
      ) ++ string("Created_timestamp_string") ++ zonedDateTime("Created_timestamp"))
        .table("Customers")

    val (customerId, dob, fName, lName, verified, createdString, createdTimestamp) =
      customers.columns

    val ALL = customerId ++ fName ++ lName ++ verified ++ dob ++ createdString ++ createdTimestamp
  }
  object Orders        {
    val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }
  object Products      {
    val products =
      (uuid("id") ++ string("name") ++ string("description") ++ string("image_url")).table("products")

    val (productId, productName, description, imageURL) = products.columns
  }
  object ProductPrices {
    val productPrices =
      (uuid("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")

    val (fkProductId, effective, price) = productPrices.columns
  }

  object OrderDetails {
    val orderDetails =
      (uuid("order_id") ++ uuid("product_id") ++ int("quantity") ++ bigDecimal("unit_price"))
        .table(
          "order_details"
        )

    val (orderDetailsOrderId, orderDetailsProductId, quantity, unitPrice) = orderDetails.columns
  }

  object DerivedTables {
    import OrderDetails._
    import Customers._
    import Orders._

    val orderDetailsDerived =
      select(orderDetailsOrderId, orderDetailsProductId, unitPrice).from(orderDetails).asTable("derived")

    val (derivedOrderId, derivedProductId, derivedUnitPrice) = orderDetailsDerived.columns
    val orderDateDerivedTable                                = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    val orderDateDerived = orderDateDerivedTable.columns
  }

  object Cities {
    case class CityId(id: Int)
    case class City(cityId: CityId, name: String, population: Int, area: Float, link: Option[String])

    import ColumnSet._

    val city = (int("id") ++ string("name") ++
      int("population") ++ float("area") ++ string("link")).table("city")

    val (cityId, cityName, population, area, link) = city.columns

    val metroSystem = (int("id") ++ int("city_id") ++ string("name") ++ int("daily_ridership")).table("metro_system")

    val (metroSystemId, cityIdFk, metroSystemName, dailyRidership) = metroSystem.columns

    val metroLine =
      (int("id") ++ int("system_id") ++ string("name") ++ int("station_count") ++ int("track_type")).table("metro_line")

    val (metroLineId, systemId, metroLineName, stationCount, trackType) = metroLine.columns
  }
}
