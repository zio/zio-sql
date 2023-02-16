package zio.sql.mysql

import java.time._
import java.sql.ResultSet
import java.util.UUID
import zio.sql.Sql
import zio.sql.select._ 
import zio.sql.expr._ 
import zio.sql.typetag._

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
