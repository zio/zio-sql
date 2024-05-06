package zio.sql

import java.time.LocalDate

import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.schema.StandardType
import zio.sql.table._
import zio.sql.insert._
import zio.sql.select._
import zio.sql.update._
import zio.sql.delete._

object ProductSchema {
  val sql = new Sql { self =>
    override def renderDelete(delete: Delete[_]): String                     = ???
    override def renderRead(read: Read[_]): String                           = ???
    override def renderUpdate(update: Update[_]): String                     = ???
    override def renderInsert[A: Schema](insert: Insert[_, A]): SqlStatement = ???
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

  implicit val localDateSchema: Schema[LocalDate] =
    Schema.primitive[LocalDate](StandardType.LocalDateType)

  implicit val productsSchema: Schema.CaseClass6[String, LocalDate, String, Int, Int, Boolean, Product] =
    DeriveSchema.gen[Product]

  val productTable = Table.defineTable[Product]

  val (id, lastUpdated, name, baseAmount, finalAmount, deleted) = productTable.columns

  val selectAll = select(id, lastUpdated, baseAmount, deleted) from productTable
}
