package zio.sql

import zio.test.Assertion.anything
import zio.test.{ assert, DefaultRunnableSpec }

object BitwiseOpSpec extends DefaultRunnableSpec {
  import ProductSchema._

  def spec = suite("Bitwise operators")(
    test("~ works on integer columns") {
      val query = selectAll.where((~baseAmount) > 0)
      assert(query)(anything)
    },
    test("& works on integer columns") {
      val query = selectAll.where((baseAmount & finalAmount) > 0)
      assert(query)(anything)
    },
    test("| works on integer columns") {
      val query = selectAll.where((baseAmount | finalAmount) > 0)
      assert(query)(anything)
    }
  )
}
