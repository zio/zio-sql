package zio.sql

import zio.test.Assertion.anything
import zio.test.assert
import zio.test.ZIOSpecDefault

class LogicalOpsSpec extends ZIOSpecDefault {
  import ProductSchema._

  implicit case object EString extends Encoder[String] {
    override def render(value: String): String = s"'$value'"
  }
  def spec = suite("Relational operators")(
    test("<= works on integer columns") {
      val query = selectAll.where(baseAmount <= finalAmount)
      assert(query)(anything)
    },
    test(">= works on integer columns") {
      val query = selectAll.where(baseAmount >= finalAmount)
      assert(query)(anything)
    },
    test("<> works on integer columns") {
      val query = selectAll.where(baseAmount <> finalAmount)
      assert(query)(anything)
    },
    test("< works on integer columns") {
      val query = selectAll.where(baseAmount < finalAmount)
      assert(query)(anything)
    },
    test("> works on integer columns") {
      val query = selectAll.where(baseAmount > finalAmount)
      assert(query)(anything)
    },
    test("=== works on integer columns") {
      val query = selectAll.where(baseAmount === finalAmount)
      assert(query)(anything)
    },
    test("not works on boolean column") {
      val selectNotDeleted = selectAll.where(deleted.not)
      assert(selectNotDeleted)(anything)
    },
    test("like works on a string column") {
      val query = selectAll.where(name like "%")
      assert(query)(anything)
    }
  )
}
