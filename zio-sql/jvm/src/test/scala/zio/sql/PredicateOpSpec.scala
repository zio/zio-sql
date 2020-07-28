package zio.sql

import zio.test.{DefaultRunnableSpec, assert, suite, test}
import zio.test.Assertion.anything

object PredicateOpSpec extends DefaultRunnableSpec {
  import ProductSchema._

  def spec = suite("Unary operators")(
    test("isTrue works on boolean column") {
      val selectDeleted = selectAll.where( deleted.isTrue )
      assert(selectDeleted)(anything)
    },
    test("isNotTrue works on boolean column") {
      val selectDeteltedNotTrue = selectAll.where( deleted.isNotTrue )
      assert(selectDeteltedNotTrue)(anything)
    },
    test("isNotNull works on column with any type") {
      val selectProperlyDefined = selectAll.where( name.isNotNull )
      assert(selectProperlyDefined)(anything)
    },
    test("isNull works on column with any type") {
      val selectBadlyDefined = selectAll.where( name.isNull )
      assert(selectBadlyDefined)(anything)
    }
  )
}
