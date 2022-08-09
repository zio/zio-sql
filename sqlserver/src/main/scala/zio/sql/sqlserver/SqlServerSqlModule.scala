package zio.sql.sqlserver

import zio.schema.Schema
import zio.sql.Sql

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

      case class CrossOuterApplyTable[A, B](
        crossType: CrossType,
        left: Table.Aux[A],
        right: Table.Aux[B]
      ) extends SqlServerTable[A with B] { self =>

        override type ColumnHead = left.ColumnHead

        override type HeadIdentity0 = left.HeadIdentity0
        override type ColumnTail    =
          left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail, right.HeadIdentity0]]

        override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] =
          left.columnSet ++ right.columnSet

        override val columnToExpr: ColumnToExpr[A with B] = new ColumnToExpr[A with B] {
          def toExpr[C](column: Column[C]): Expr[Features.Source[column.Identity, A with B], A with B, C] =
            if (left.columnSet.contains(column))
              left.columnToExpr
                .toExpr(column)
                .asInstanceOf[Expr[Features.Source[column.Identity, A with B], A with B, C]]
            else
              right.columnToExpr
                .toExpr(column)
                .asInstanceOf[Expr[Features.Source[column.Identity, A with B], A with B, C]]
        }
      }

      implicit def tableSourceToSelectedBuilder[A](
        table: Table.Aux[A]
      ): CrossOuterApplyTableBuilder[A] =
        new CrossOuterApplyTableBuilder(table)

      sealed case class CrossOuterApplyTableBuilder[A](left: Table.Aux[A]) {
        self =>

        final def crossApply[Out](
          right: Table.DerivedTable[Out, Read[Out]]
        ): Table.DialectSpecificTable[A with right.TableType] = {

          val tableExtension = CrossOuterApplyTable[A, right.TableType](
            CrossType.CrossApply,
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }

        final def outerApply[Out](
          right: Table.DerivedTable[Out, Read[Out]]
        ): Table.DialectSpecificTable[A with right.TableType] = {

          val tableExtension = CrossOuterApplyTable[A, right.TableType](
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
