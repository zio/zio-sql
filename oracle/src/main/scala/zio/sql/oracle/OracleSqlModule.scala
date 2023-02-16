package zio.sql.oracle

import zio.sql.Sql
import java.time.YearMonth
import java.sql.ResultSet
import scala.util.Try
import java.time.Duration
import zio.sql.select._
import zio.sql.expr._
import zio.sql.typetag._

trait OracleSqlModule extends Sql { self =>

  trait OracleTypeTag[+A] extends TypeTag.TypeTagExtension[A]

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
