package zio.sql.oracle

import zio.sql.Jdbc

trait ShopSchema extends Jdbc { self =>
  import self.ColumnSet._

  object Customers {

    val customers =
      (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name") ++
        boolean("verified") ++ zonedDateTime("Created_timestamp"))
        .table("customers")

    val (customerId, dob, fName, lName, verified, createdTimestamp) = customers.columns
  }
  object Orders        {
    val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

    val (orderId, fkCustomerId, orderDate) = orders.columns
  }
  object Products      {
    val products =
      (int("id") ++ string("name") ++ string("description") ++ string("image_url")).table("products")

    val (productId, productName, description, imageURL) = products.columns
  }
  object ProductPrices {
    val productPrices =
      (int("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")

    val (fkProductId, effective, price) = productPrices.columns
  }

  object OrderDetails {
    val orderDetails =
      (int("order_id") ++ int("product_id") ++ double("quantity") ++ double("unit_price"))
        .table(
          "order_details"
        ) // todo fix #3 quantity should be int, unit price should be bigDecimal, numeric operators only support double ATM.

    val (fkOrderId, fkProductId, quantity, unitPrice) = orderDetails.columns
  }
}
