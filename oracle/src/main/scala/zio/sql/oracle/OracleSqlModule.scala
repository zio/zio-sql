package zio.sql.oracle

import zio.sql.Sql
import zio.schema.Schema
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

trait OracleSqlModule extends Sql { self =>

  object OracleFunctionDef {
    val Sind = FunctionDef[Double, Double](FunctionName("sind"))
  }

  implicit val localDateSchema =
    Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))

  implicit val localDateTimeSchema =
    Schema.primitive[LocalDateTime](zio.schema.StandardType.LocalDateTimeType(DateTimeFormatter.ISO_DATE_TIME))
}
