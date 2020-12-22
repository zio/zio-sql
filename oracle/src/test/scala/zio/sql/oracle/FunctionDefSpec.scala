package zio.sql.oracle

//import java.time._
//import java.time.format.DateTimeFormatter
//import java.util.UUID
import zio.Cause
//import zio.random.{Random => ZioRandom}
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect.{timeout}
import zio.duration._

object FunctionDefSpec extends OracleRunnableSpec with ShopSchema {

  import Customers._
  import FunctionDef.{ CharLength => _, _ }
//  import OracleFunctionDef._

  private def collectAndCompare(
    expected: Seq[String],
    testResult: ZStream[FunctionDefSpec.ReadExecutor, Exception, String]
  ): zio.ZIO[FunctionDefSpec.Environment, Any, TestResult] = {
    val assertion = for {
      r <- testResult.runCollect
    } yield assert(r.toList)(equalTo(expected))

    assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
  }

//  private val timestampFormatter = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSSS").withZone(ZoneId.of("UTC"))

  val spec = suite("Oracle FunctionDef")(
    testM("simple select") {
      import Expr._
      //note: a plain number (3) would and should not compile
      val query = select(ConcatWs4("+", "1", "2", "3")) from customers
      println(renderRead(query))

      val expected = Seq( // note: one for each row
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3",
        "1+2+3"
      )

      val testResult = execute(query).to[String, String](identity)
      collectAndCompare(expected, testResult)
    }
  ) @@ timeout(5.minutes)
}
