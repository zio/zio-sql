package zio.sql.oracle

trait ShopSchema extends OracleSqlModule { self =>
  import self.ColumnSet._

  object Customers {

    val customers =
      (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name") ++
        boolean("verified"))
        .table("customers")

    val (customerId, dob, fName, lName, verified) = customers.columns
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

  object AllTypes {
    val allTypes =
      (uuid("id") ++
        byteArray("bytearray") ++
        bigDecimal("bigdecimal") ++
        boolean("boolean_") ++
        char("char_") ++
        double("double_") ++
        float("float_") ++
        instant("instant") ++
        int("int_") ++
        (int("optional_int") @@ ColumnSetAspect.nullable) ++
        localDate("localdate") ++
        localDateTime("localdatetime") ++
        localTime("localtime") ++
        long("long_") ++
        offsetDateTime("offsetdatetime") ++
        offsetTime("offsettime") ++
        short("short") ++
        string("string") ++
        uuid("uuid") ++
        zonedDateTime("zoneddatetime") ++
        yearMonth("yearmonth") ++
        duration("duration")).table("all_types")

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
      zonedDatetimeCol,
      yearMonthCol,
      durationCol
    ) = allTypes.columns
  }
}
