package zio.sql.sqlserver

import java.math.BigDecimal
import zio.sql.table._
import zio.sql.select._
import zio.sql.Sql
import zio.sql.expr.FunctionName
import zio.sql.expr.AggregationDef

trait SqlServerSqlModule extends Sql { self =>

  object SqlServerSpecific {

    sealed trait SqlServerTable[A] extends Table.TableExtension[A]

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
}
