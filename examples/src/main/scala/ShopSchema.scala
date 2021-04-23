package zio.sql

trait ShopSchema extends Jdbc { self =>
  import self.ColumnSet._

  object Users         {
    val users = (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name")).table("users")

    val userId :*: dob :*: fName :*: lName :*: _ = users.columns
  }
  object Orders        {
    val orders = (uuid("id") ++ uuid("usr_id") ++ localDate("order_date")).table("orders")

    val orderId :*: fkUserId :*: orderDate :*: _ = orders.columns
  }
  object Products      {
    val products =
      (int("id") ++ string("name") ++ string("description") ++ string("image_url")).table("products")

    val productId :*: description :*: imageURL :*: _ = products.columns
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
