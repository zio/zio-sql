package zio.sql.mysql

import zio.sql.expr._
import zio.sql.ops.Operator.RelationalOp
import zio.sql.select._
import zio.sql.typetag._
import zio.sql.{ Features, Sql }

import java.sql.ResultSet
import java.time._
import java.util.UUID

trait MysqlSqlModule extends Sql { self =>

  object MysqlSpecific {
    trait MysqlTypeTag[+A] extends TypeTag.TypeTagExtension[A]

    object MysqlTypeTag {
      implicit case object TYear extends MysqlTypeTag[Year] {
        override def decode(column: Int, resultSet: ResultSet): Either[DecodingError, Year] =
          scala.util
            .Try(Year.of(resultSet.getByte(column).toInt))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              r => Right(r)
            )
      }
    }

    implicit class ExprOps[F1, A1, B](expr: Expr[F1, A1, B]) {
      def soundsLike[F2, A2 <: A1](that: Expr[F2, A2, B])(implicit ev: B <:< String): Expr[F1 with F2, A2, Boolean] =
        Expr.Relational(expr, that, RelationalOp.MySqlExtensions.SoundsLike)
    }

    implicit class LiteralOps[B](line: B)(implicit literal: B => Expr[Features.Literal, Any, B]) {
      def soundsLike[F, A](that: Expr[F, A, B])(implicit
        ev: B <:< String
      ): Expr[Features.Literal with F, A, Boolean] = literal(line).soundsLike(that)
    }
  }

  object MysqlFunctionDef {
    val BitLength   = FunctionDef[String, Int](FunctionName("bit_length"))
    val Crc32       = FunctionDef[String, Long](FunctionName("crc32"))
    val CurrentDate = Expr.ParenlessFunctionCall0[LocalDate](FunctionName("current_date"))
    val CurrentTime = Expr.ParenlessFunctionCall0[OffsetTime](FunctionName("current_time"))
    val Degrees     = FunctionDef[Double, Double](FunctionName("degrees"))
    val Hex         = FunctionDef[Long, String](FunctionName("hex"))
    val Log2        = FunctionDef[Double, Double](FunctionName("log2"))
    val Log10       = FunctionDef[Double, Double](FunctionName("log10"))
    val MakeDate    = FunctionDef[(Int, Int), LocalDate](FunctionName("makedate"))
    val MakeTime    = FunctionDef[(Int, Int, Double), LocalTime](FunctionName("maketime"))
    val Now         = FunctionDef[Any, ZonedDateTime](FunctionName("now"))
    val Pi          = Expr.FunctionCall0[Double](FunctionDef[Any, Double](FunctionName("pi")))
    val Soundex     = FunctionDef[String, String](FunctionName("soundex"))
    val Rand        = FunctionDef[Int, Double](FunctionName("rand"))
    val RPad        = FunctionDef[(String, Int, String), String](FunctionName("rpad"))
    val Uuid        = Expr.FunctionCall0[UUID](FunctionDef[Any, UUID](FunctionName("uuid")))
    val Radians     = FunctionDef[Double, Double](FunctionName("radians"))
  }
}
