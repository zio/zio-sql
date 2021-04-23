package zio.sql

import zio.test.Assertion.anything
import zio.test.{ assert, DefaultRunnableSpec }

object ArithmeticOpsSpec extends DefaultRunnableSpec {
  import ProductSchema._

  def spec = suite("Arithmetic operators")(
    test("+ works on integer columns") {
      val query = selectAll.where(baseAmount + finalAmount > 42)
      assert(query)(anything)
    },
    test("- works on integer columns") {
      val query = selectAll.where(baseAmount - finalAmount > 42)
      assert(query)(anything)
    },
    test("* works on integer columns") {
      val query = selectAll.where(baseAmount * finalAmount > 42)
      assert(query)(anything)
    },
    test("/ works on integer columns") {
      val query = selectAll.where(baseAmount / finalAmount > 42)
      assert(query)(anything)
    },
    test("- works on integer column") {
      val selectNotDeleted = selectAll.where(-baseAmount < 0)
      assert(selectNotDeleted)(anything)
    }
  )
}
