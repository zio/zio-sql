package zio.sql

import zio.schema.Schema
import java.time.LocalDate
import zio.schema.DeriveSchema
import zio.schema.StandardType

object ProductSchema {
  val sql = new Sql { self =>
    override def renderDelete(delete: self.Delete[_]): String = ???
    override def renderRead(read: self.Read[_]): String       = ???
    override def renderUpdate(update: self.Update[_]): String = ???

    override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???
  }

  import sql._

  case class Product(
    id: String,
    last_updated: LocalDate,
    name: String,
    base_amount: Int,
    final_amount: Int,
    deleted: Boolean
  )

  implicit val localDateSchema =
    Schema.primitive[LocalDate](StandardType.LocalDateType)

  implicit val productsSchema = DeriveSchema.gen[Product]

  val productTable = defineTable[Product]

  val (id, lastUpdated, name, baseAmount, finalAmount, deleted) = productTable.columns

  val selectAll = select(id, lastUpdated, baseAmount, deleted) from productTable
}
