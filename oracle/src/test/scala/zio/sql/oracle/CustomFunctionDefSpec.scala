package zio.sql.oracle

import zio.test.Assertion._
import zio.test.TestAspect.timeout
import zio.test._
import zio._

object CustomFunctionDefSpec extends OracleRunnableSpec with DualSchema {
  import OracleFunctionDef._

  import Dual._

  override def specLayered = suite("Oracle FunctionDef")(
    test("ltrim2") {
      assertZIO(execute(select(Ltrim2("$## foo$#", "#$")).from(dual)).runHead.some)(
        equalTo(" foo$#")
      )
    },
    test("rtrim2") {
      assertZIO(execute(select(Rtrim2("$#foo $##", "#$")).from(dual)).runHead.some)(
        equalTo("$#foo ")
      )
    }
  ) @@ timeout(5.minutes)
}
