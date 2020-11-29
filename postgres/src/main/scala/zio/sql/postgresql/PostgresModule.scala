package zio.sql.postgresql

import java.time._
import zio.sql.Jdbc
import zio.sql.rendering.{ Builder, RenderModule, Rendering }

import scala.language.implicitConversions

/**
 */
trait PostgresModule extends Jdbc with PostgresRenderModule { self =>

  override type SqlRendering[A] = PostgresRendering[A]

  override type ExprExtensionType[F, -A, B] = PostgresExprExtension[F, A, B]

  sealed trait PostgresExprExtension[F, -A, B]
  object PostgresExprExtension {
    implicit def postgresExpr2Expr[F, A, B: TypeTag](pgExpr: PostgresExprExtension[F, A, B]) =
      Expr.ExprDialectSpecific(pgExpr)

    implicit def postgresExpr2Selection[F, A, B: TypeTag](
      pgExpr: PostgresExprExtension[F, A, B]
    ): Selection[F, A, SelectionSet.Cons[A, B, SelectionSet.Empty]] =
      Selection.computedOption(pgExpr, Expr.exprName(pgExpr))

    sealed case class Overlay[F1, F2, F3, F4, Source, String](
      main: Expr[F1, Source, String],
      replacing: Expr[F2, Source, String],
      starting: Expr[F3, Source, Int],
      number: Expr[F4, Source, Int]
    ) extends PostgresExprExtension[F1 :||: F2 :||: F3 :||: F4, Source, String] {
      val functionName = "overlay"
    }
  }
  object PostgresFunctionDef   {
    val IsFinite                    = FunctionDef[Instant, Boolean](FunctionName("isfinite"))
    val TimeOfDay                   = FunctionDef[Any, String](FunctionName("timeofday"))
    val CurrentTime                 = Expr.ParenlessFunctionCall0[OffsetTime](FunctionName("current_time"))
    val CharLength                  = FunctionDef[String, Int](FunctionName("character_length"))
    val Localtime                   = Expr.ParenlessFunctionCall0[LocalTime](FunctionName("localtime"))
    val LocaltimeWithPrecision      = FunctionDef[Int, LocalTime](FunctionName("localtime"))
    val Localtimestamp              = Expr.ParenlessFunctionCall0[Instant](FunctionName("localtimestamp"))
    val LocaltimestampWithPrecision = FunctionDef[Int, Instant](FunctionName("localtimestamp"))
    val Md5                         = FunctionDef[String, String](FunctionName("md5"))
    val ParseIdent                  = FunctionDef[String, String](FunctionName("parse_ident"))
    val Chr                         = FunctionDef[Int, String](FunctionName("chr"))
    val CurrentDate                 = Expr.ParenlessFunctionCall0[LocalDate](FunctionName("current_date"))
    val Initcap                     = FunctionDef[String, String](FunctionName("initcap"))
    val Repeat                      = FunctionDef[(String, Int), String](FunctionName("repeat"))
    val Reverse                     = FunctionDef[String, String](FunctionName("reverse"))
    val TrimScale                   = FunctionDef[Double, Double](FunctionName("trim_scale"))
    val Hex                         = FunctionDef[Int, String](FunctionName("to_hex"))
    val Left                        = FunctionDef[(String, Int), String](FunctionName("left"))
    val Length                      = FunctionDef[String, Int](FunctionName("length"))
    val MinScale                    = FunctionDef[Double, Int](FunctionName("min_scale"))
    val Radians                     = FunctionDef[Double, Double](FunctionName("radians"))
    val Right                       = FunctionDef[(String, Int), String](FunctionName("right"))
    val StartsWith                  = FunctionDef[(String, String), Boolean](FunctionName("starts_with"))
    val Translate                   = FunctionDef[(String, String, String), String](FunctionName("translate"))
    val Trunc                       = FunctionDef[Double, Double](FunctionName("trunc"))
    val Sind                        = FunctionDef[Double, Double](FunctionName("sind"))
    val GCD                         = FunctionDef[(Double, Double), Double](FunctionName("gcd"))
    val LCM                         = FunctionDef[(Double, Double), Double](FunctionName("lcm"))
    val CBRT                        = FunctionDef[Double, Double](FunctionName("cbrt"))
    val Degrees                     = FunctionDef[Double, Double](FunctionName("degrees"))
    val Div                         = FunctionDef[(Double, Double), Double](FunctionName("div"))
    val Factorial                   = FunctionDef[Int, Int](FunctionName("factorial"))
    val Random                      = FunctionDef[Any, Double](FunctionName("random"))
    val LPad                        = FunctionDef[(String, Int, String), String](FunctionName("lpad"))
    val RPad                        = FunctionDef[(String, Int, String), String](FunctionName("rpad"))
    val ToTimestamp                 = FunctionDef[Long, ZonedDateTime](FunctionName("to_timestamp"))
    val PgClientEncoding            = FunctionDef[Any, String](FunctionName("pg_client_encoding"))
  }

  override def renderRead(read: self.Read[_]): String = {
    implicit val builder: Builder = Builder()
    render(read)
    println(builder.toString)
    builder.toString
  }

  def renderUpdate(update: Update[_]): String = {
    implicit val builder: Builder = Builder()
    render(update)
    println(builder.toString)
    builder.toString
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val builder: Builder = Builder()
    render(delete)
    println(builder.toString)
    builder.toString
  }

}
