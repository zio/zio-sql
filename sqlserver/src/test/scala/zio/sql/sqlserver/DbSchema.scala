package zio.sql.sqlserver

import zio.sql.Jdbc

trait DbSchema extends Jdbc { self =>
  import self.ColumnSet._

  object DbSchema {

    val customers =
      (uuid("id") ++ string("first_name") ++ string("last_name") ++ boolean("verified") ++ localDate("dob"))
        .table("customers")

    val customerId :*: fName :*: lName :*: verified :*: dob :*: _ =
      customers.columns

    val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

    val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns

    val orderDetails =
      (uuid("order_id") ++ uuid("product_id") ++ int("quantity") ++ double("unit_price")).table("order_details")

    val orderDetailsId :*: productId :*: quantity :*: unitPrice :*: _ = orderDetails.columns

    val orderDetailsDerived = select(orderDetailsId ++ productId ++ unitPrice).from(orderDetails).asTable("derived")

    val derivedOrderId :*: derivedProductId :*: derivedUnitPrice :*: _ = orderDetailsDerived.columns

    val orderDateDerivedTable = customers
      .subselect(orderDate)
      .from(orders)
      .limit(1)
      .where(customerId === fkCustomerId)
      .orderBy(Ordering.Desc(orderDate))
      .asTable("derived")

    val orderDateDerived :*: _ = orderDateDerivedTable.columns
  }
}
