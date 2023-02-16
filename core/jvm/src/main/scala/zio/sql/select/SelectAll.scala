package zio.sql.select

import zio.sql.table._

final case class SelectAll() {

  // TODO check if helper's types can be moved to table or rewrite to macro
  def from[A](table: Table.Source.Aux[A])(implicit helper: SelectAllHelper[table.ColumnsOut, A]): Read.Select[
    helper.F,
    helper.ResultTypeRepr,
    A,
    helper.ColumnHead,
    helper.SelectionTail
  ] = {
    type B0 = SelectionSet.ConsAux[
      helper.ResultTypeRepr,
      A,
      helper.ColumnHead,
      helper.SelectionTail
    ]
    val b: B0 = ??? // table.all.selection.value.asInstanceOf[B0]

    Read.Subselect[helper.F, helper.ResultTypeRepr, A, A, helper.ColumnHead, helper.SelectionTail](
      Selection[helper.F, A, B0](b),
      Some(table),
      true
    )
  }
}
