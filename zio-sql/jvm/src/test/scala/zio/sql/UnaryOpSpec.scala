package zio.sql

import zio.test.{assert, DefaultRunnableSpec, suite, test}
import zio.test.Assertion.anything

object ProductSchema {
  val userSql = new Sql { self =>
    import self.ColumnSet._

    val productTable = (
      string("id") ++
        localDate("last_updated") ++
        string("name") ++
        int("amount") ++
        boolean("deleted")
      ).table("product")

    val id :*: lastUpdated :*: name :*: amount :*: deleted :*: _ = productTable.columns

    val selectAll = select { id ++ lastUpdated ++ amount ++ deleted } from productTable

    val selectDeteltedNotTrue = selectAll.where( deleted.isNotTrue )
    val selectDeleted = selectAll.where( deleted.isTrue )
    val selectNotDeleted = selectAll.where( deleted.not )

    val selectProperlyDefined = selectAll.where( name.isNotNull )
    val selectBadlyDefined = selectAll.where( name.isNull )
  }
}

object UnaryOpSpec extends DefaultRunnableSpec {
  import ProductSchema.userSql._

  def spec = suite("Unary operators")(
    test("isTrue works on boolean column") {
      assert(selectDeleted)(anything)
    },
    test("isNotTrue works on boolean column") {
      assert(selectDeteltedNotTrue)(anything)
    },

    test("not works on boolean column") {
      assert(selectNotDeleted)(anything)
    },

    test("isNotNull works on column with any type") {
      assert(selectProperlyDefined)(anything)
    },
    test("isNull works on column with any type") {
      assert(selectBadlyDefined)(anything)
    }
  )
}
