package zio.sql.oracle

import zio.sql.Sql
import zio.schema.Schema
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.YearMonth
import java.sql.ResultSet
import scala.util.Try
import java.time.Duration
import java.time.ZonedDateTime
import java.time.LocalTime
import java.time.Instant
import java.time.OffsetTime
import java.time.OffsetDateTime

trait OracleSqlModule extends Sql { self =>
  import ColumnSet._

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

  implicit val instantSchema =
    Schema.primitive[Instant](zio.schema.StandardType.InstantType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  implicit val localDateSchema =
    Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))

  implicit val localTimeSchema =
    Schema.primitive[LocalTime](zio.schema.StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME))

  implicit val localDateTimeSchema =
    Schema.primitive[LocalDateTime](zio.schema.StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  implicit val offsetTimeSchema =
    Schema.primitive[OffsetTime](zio.schema.StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME))

  implicit val offsetDateTimeSchema =
    Schema.primitive[OffsetDateTime](zio.schema.StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  implicit val zonedDatetimeSchema =
    Schema.primitive[ZonedDateTime](zio.schema.StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  def yearMonth(name: String): Singleton[YearMonth, name.type] = singleton[YearMonth, name.type](name)

  def duration(name: String): Singleton[Duration, name.type] = singleton[Duration, name.type](name)
}
