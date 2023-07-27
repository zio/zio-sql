package zio.sql.select

import zio.sql.table._
import zio.sql.expr._
import scala.language.experimental.macros
import scala.language.implicitConversions

final case class SelectAll() {
  def from[AllColumnsIdentities, TableType, ColumnsOut, F, Repr, Head, Tail <: SelectionSet[TableType]](
    wrapper: SelectAllWrapper[AllColumnsIdentities, TableType, ColumnsOut]
  ): Read.Subselect[F, Repr, TableType, TableType, Head, Tail] =
    macro SelectionMacro.buildSelectAll[AllColumnsIdentities, ColumnsOut, TableType]
}

case class SelectAllWrapper[AllColumnsIdentities, TableType, ColumnsOut](
  table: Table.Source.WithTableDetails[AllColumnsIdentities, TableType, ColumnsOut],
  exprs: List[zio.sql.expr.Expr[_, TableType, _]]
)

object SelectAllWrapper {
  implicit def tableToExprs[AllColumnsIdentities, TableType, ColumnsOut](
    table: Table.Source.WithTableDetails[AllColumnsIdentities, TableType, ColumnsOut]
  ): SelectAllWrapper[AllColumnsIdentities, TableType, ColumnsOut] =
    SelectAllWrapper(
      table,
      table.columns.asInstanceOf[Product].productIterator.toList.map(_.asInstanceOf[Expr[_, TableType, _]])
    )
}
