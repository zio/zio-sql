package zio.sql.mysql

import zio.sql.Jdbc

trait ShopSchema extends Jdbc { self =>
  import self.ColumnSet._

  object Customers     {
    val customers =
      (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name") ++ boolean("verified"))
        .table("customers")

    val customerId :*: dob :*: fName :*: lName :*: verified :*: _ = customers.columns
  }
  object Orders        {
    val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

    val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns
  }
  object Products      {
    val columns   = uuid("id") ++ string("name") ++ string("description") ++ string("image_url") ++ uuid("parent_id")
    val products  = columns.table("products")
    val products2 = columns.table("products")

    val productId :*: productName :*: description :*: imageURL :*: parentId :*: _ = products.columns
  }
  object ProductPrices {
    val productPrices =
      (int("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")

    val fkProductId :*: effective :*: price :*: _ = productPrices.columns
  }

  object OrderDetails {
    val orderDetails =
      (int("order_id") ++ int("product_id") ++ double("quantity") ++ double("unit_price"))
        .table(
          "order_details"
        ) //todo fix #3 quantity should be int, unit price should be bigDecimal, numeric operators only support double ATM.

    val fkOrderId :*: fkProductId :*: quantity :*: unitPrice :*: _ = orderDetails.columns
  }
}
