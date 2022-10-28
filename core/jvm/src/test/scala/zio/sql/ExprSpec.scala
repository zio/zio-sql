package zio.sql

import zio.test.Assertion.anything
import zio.test.{ZIOSpecDefault, assert}

object ExprSpec extends ZIOSpecDefault {
  import ProductSchema._

  def spec = suite("Aggregate Expression")(
    test("+ works on integer, Double and BigDecimal columns") {
      val query = agSelect
      assert(query)(anything)
    }

  )
}

