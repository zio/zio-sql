package zio.sql.mysql

import zio.Scope
import zio.schema.DeriveSchema
import zio.sql.table.Table._
import zio.test._

import java.time._
import java.util.UUID

object MySqlQueryShowSpec extends ZIOSpecDefault with MysqlRenderModule {
  final case class Product(id: UUID, name: String, price: Double)

  object Product {
    implicit val productSchema = DeriveSchema.gen[Product]
    val products               = defineTableSmart[Product]
    val (id, name, price)      = products.columns
  }

  final case class Order(id: UUID, productId: UUID, quantity: Int, orderDate: LocalDate)

  object Order {
    implicit val orderSchema                 = DeriveSchema.gen[Order]
    val orders                               = defineTable[Order]
    val (orderId, productId, quantity, date) = orders.columns
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("MySqlQueryShow")(
    test("rendering select") {
      import Order._
      import Product._

      val selectQueryRender =
        select(orderId, name)
          .from(
            products
              .join(orders)
              .on(productId === id)
          )
          .limit(5)
          .offset(10)
          .show

      val expectedQuery =
        "SELECT order.id, products.name FROM products INNER JOIN order ON order.product_id = products.id  LIMIT 5 OFFSET 10"

      assertTrue(selectQueryRender == expectedQuery)
    },
    test("rendering insert") {
      import Product._

      def insertProduct(uuid: UUID) =
        insertInto(products)(id, name, price)
          .values((uuid, "Zionomicon", 10.5))

      val expectedQuery = "INSERT INTO products (id, name, price) VALUES (?, ?, ?);"

      assertTrue(insertProduct(UUID.fromString("dd5a7ae7-de19-446a-87a4-576d79de5c83")).show == expectedQuery)
    },
    test("rendering update") {
      import Product._

      def updateProduct(uuid: UUID) =
        update(products)
          .set(name, "foo")
          .set(price, price * 1.1)
          .where(id === uuid)

      val expectedQuery =
        "UPDATE products SET products.name = 'foo', products.price = products.price * 1.1 WHERE products.id = 'f1e69839-964f-44b7-b90d-bd5f51700540'"

      assertTrue(updateProduct(UUID.fromString("f1e69839-964f-44b7-b90d-bd5f51700540")).show == expectedQuery)
    },
    test("rendering delete") {
      import Product._

      def deleteProduct(uuid: UUID) =
        deleteFrom(products)
          .where(id === uuid)

      val expectedQuery = "DELETE FROM products WHERE products.id = '95625b37-e785-4b4f-86b1-69affaf5f848'"

      assertTrue(deleteProduct(UUID.fromString("95625b37-e785-4b4f-86b1-69affaf5f848")).show == expectedQuery)
    }
  )
}
