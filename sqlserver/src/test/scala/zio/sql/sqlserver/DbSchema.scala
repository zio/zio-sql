package zio.sql.sqlserver

import zio.sql.Jdbc

trait DbSchema extends Jdbc { self =>
  import self.ColumnSet._

  object DbSchema {

    val customers =
      (uuid("id") ++ string("first_name") ++ string("last_name") ++ boolean("verified") ++ localDate("dob"))
        .table("customers")

    val (customerId, fName, lName, verified, dob) =
      customers.columns

    val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

    val (orderId, fkCustomerId, orderDate) = orders.columns

    val productPrices =
      (uuid("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")

    val (fkProductId, effective, price) = productPrices.columns

    val orderDetails =
      (uuid("order_id") ++ uuid("product_id") ++ int("quantity") ++ bigDecimal("unit_price")).table("order_details")

    val (orderDetailsId, productId, quantity, unitPrice) = orderDetails.columns

    val orderDetailsDerived = select(orderDetailsId, productId, unitPrice).from(orderDetails).asTable("derived")

    val (derivedOrderId, derivedProductId, derivedUnitPrice) = orderDetailsDerived.columns

    val orderDateDerivedTable = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    val orderDateDerived = orderDateDerivedTable.columns

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
        zonedDateTime("zoneddatetime")).table("all_types")

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
