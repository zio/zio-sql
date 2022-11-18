package zio.sql.oracle

import zio.sql.Sql
import java.time.YearMonth
import java.sql.ResultSet
import scala.util.Try
import java.time.Duration

trait OracleSqlModule extends Sql { self =>
  override type TypeTagExtension[+A] = OracleTypeTag[A]

  trait OracleTypeTag[+A] extends Tag[A] with Decodable[A]

  object OracleTypeTag {
    implicit case object TYearMonth extends OracleTypeTag[YearMonth] {
      def decode(column: Int, resultSet: ResultSet): Either[DecodingError, YearMonth] =
        Try(YearMonth.parse(resultSet.getString(column)))
          .fold(
            _ => Left(DecodingError.UnexpectedNull(column)),
            r => Right(r)
          )
    }
    implicit case object TDuration  extends OracleTypeTag[Duration]  {
      def decode(column: Int, resultSet: ResultSet): Either[DecodingError, Duration] =
        Try(Duration.parse(resultSet.getString(column)))
          .fold(
            _ => Left(DecodingError.UnexpectedNull(column)),
            r => Right(r)
          )
    }
  }

  object OracleFunctionDef {
    val Sind = FunctionDef[Double, Double](FunctionName("sind"))
  }
}
