package zio.sql

import zio.test.{ assert, DefaultRunnableSpec }
import zio.test.Assertion.anything

object PredicateOpSpec extends DefaultRunnableSpec {
  import ProductSchema._

  def spec = suite("Unary operators")(
    test("isTrue works on boolean column") {
      val query = selectAll.where(deleted.isTrue)
      assert(query)(anything)
    },
    test("isNotTrue works on boolean column") {
      val query = selectAll.where(deleted.isNotTrue)
      assert(query)(anything)
    },
    test("isNotNull works on column with any type") {
      val query = selectAll.where(name.isNotNull)
      assert(query)(anything)
    },
    test("isNull works on column with any type") {
      val query = selectAll.where(name.isNull)
      assert(query)(anything)
    }
  )
}
