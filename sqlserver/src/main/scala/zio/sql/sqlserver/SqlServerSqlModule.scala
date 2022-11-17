package zio.sql.sqlserver

import zio.schema.Schema
import zio.sql.Sql
import java.math.BigDecimal
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime }
import java.time.format.DateTimeFormatter

trait SqlServerSqlModule extends Sql { self =>

  override type TableExtension[A] = SqlServerSpecific.SqlServerTable[A]

  object SqlServerSpecific {

    sealed trait SqlServerTable[A] extends Table.TableEx[A]

    object SqlServerTable {

      import scala.language.implicitConversions

      sealed trait CrossType
      object CrossType {
        case object CrossApply extends CrossType
        case object OuterApply extends CrossType
      }

      sealed case class CrossOuterApplyTable[A, B](
        crossType: CrossType,
        left: Table.Aux[A],
        right: Table.Aux[B]
      ) extends SqlServerTable[A with B]

      implicit def tableSourceToSelectedBuilder[A](
        table: Table.Aux[A]
      ): CrossOuterApplyTableBuilder[A] =
        new CrossOuterApplyTableBuilder(table)

      sealed case class CrossOuterApplyTableBuilder[A](left: Table.Aux[A]) {
        self =>

        final def crossApply[Reprs, Out, RightSource](
          right: Table.DerivedTable[Reprs, Out, Read.WithReprs[Out, Reprs], RightSource]
        ): Table.DialectSpecificTable[A with RightSource] = {

          val tableExtension = CrossOuterApplyTable[A, RightSource](
            CrossType.CrossApply,
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }

        final def outerApply[Reprs, Out, RightSource](
          right: Table.DerivedTable[Reprs, Out, Read.WithReprs[Out, Reprs], RightSource]
        ): Table.DialectSpecificTable[A with RightSource] = {

          val tableExtension = CrossOuterApplyTable[A, RightSource](
            CrossType.OuterApply,
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }
      }
    }

    object SqlServerFunctionDef {
      val Avg = AggregationDef[BigDecimal, Int](FunctionName("avg"))
    }
  }

  implicit val localDateSchema =
    Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))

  implicit val instantSchema =
    Schema.primitive[Instant](zio.schema.StandardType.InstantType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))

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
}
