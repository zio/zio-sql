package zio.sql

import zio.schema.Schema

object ProductSchema {
  val sql = new Sql { self =>
    override def renderDelete(delete: self.Delete[_]): String = ???
    override def renderRead(read: self.Read[_]): String       = ???
    override def renderUpdate(update: self.Update[_]): String = ???

    override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???
  }
  import sql.ColumnSet._
  import sql._

  val productTable = (
    string("id") ++
      localDate("last_updated") ++
      string("name") ++
      int("base_amount") ++
      int("final_amount") ++
      boolean("deleted")
  ).table("product")

  val id :*: lastUpdated :*: name :*: baseAmount :*: finalAmount :*: deleted :*: _ = productTable.columns

  val selectAll = select(id ++ lastUpdated ++ baseAmount ++ deleted) from productTable
}
