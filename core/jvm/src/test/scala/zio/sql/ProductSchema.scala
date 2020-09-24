package zio.sql

object ProductSchema {
  val sql = new Sql {
    override def renderRead(read: this.Read[_]): String = ???
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

  val selectAll = select { id ++ lastUpdated ++ baseAmount ++ deleted } from productTable
}
