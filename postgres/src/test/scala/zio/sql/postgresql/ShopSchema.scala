package zio.sql.postgresql

import zio.sql.Jdbc

trait ShopSchema extends Jdbc { self =>
  import self.ColumnSet._

  object Persons {

    import ColumnSetAspect._

    val persons =
      (uuid("id") ++ string("first_name") ++ string("last_name") ++ (localDate("dob") @@ nullable))
        .table("persons")

    val (personId, fName, lName, dob) = persons.columns
  }

  object Customers     {
    //https://github.com/zio/zio-sql/issues/320 Once Insert is supported, we can remove created_timestamp_string
    val customers =
      (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name") ++ boolean(
        "verified"
      ) ++ string("created_timestamp_string") ++ zonedDateTime("created_timestamp"))
        .table("customers")

    val (customerId, dob, fName, lName, verified, createdString, createdTimestamp) =
      customers.columns
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
      select(orderDetailsOrderId ++ orderDetailsProductId ++ unitPrice).from(orderDetails).asTable("derived")

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
}
