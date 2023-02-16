package zio.sql.select

import zio.sql.table.Column
import zio.sql.typetag.TypeTag
import zio.sql.expr.Expr

sealed trait ColumnSelection[-Source, +ColumnType] {
  type ColumnType0 <: ColumnType

  def name: Option[String]

  val toColumn: Column[ColumnType]
}


object ColumnSelection {
  final case class Constant[ColumnType: TypeTag](value: ColumnType, name: Option[String])
      extends ColumnSelection[Any, ColumnType] {
    def typeTag: TypeTag[ColumnType] = implicitly[TypeTag[ColumnType]]

    val toColumn: Column[ColumnType] = name match {
      case Some(value) => Column.Named(value)
      case None        => Column.Indexed()
    }
  }

  final case class Computed[F, Source, ColumnType](expr: Expr[F, Source, ColumnType], name: Option[String])
      extends ColumnSelection[Source, ColumnType] {
    implicit def typeTag: TypeTag[ColumnType] = Expr.typeTagOf(expr)

    val toColumn: Column[ColumnType] = name match {
      case Some(value) => Column.Named(value)
      case None        => Column.Indexed()
    }
  }
}