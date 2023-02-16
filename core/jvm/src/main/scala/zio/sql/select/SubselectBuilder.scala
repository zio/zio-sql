package zio.sql.select

import zio.sql.table._
import zio.sql.macros._

final case class SubselectBuilder[F, Source, B <: SelectionSet[Source], ParentTable](
  selection: Selection[F, Source, B]
) {
  def from[Source0](table: Table.Aux[Source0])(implicit
    ev1: Source0 with ParentTable <:< Source,
    ev2: B <:< SelectionSet.Cons[Source, selection.value.ColumnHead, selection.value.SelectionTail],
    normalizer: Normalizer[selection.value.ResultTypeRepr]
  ): Read.Subselect[
    F,
    normalizer.Out,
    Source with ParentTable,
    Source0,
    selection.value.ColumnHead,
    selection.value.SelectionTail
  ] = {
    type B0 = SelectionSet.ConsAux[
      selection.value.ResultTypeRepr,
      Source with ParentTable,
      selection.value.ColumnHead,
      selection.value.SelectionTail
    ]
    val b: B0 = selection.value.asInstanceOf[B0]

    Read.Subselect(Selection[F, Source with ParentTable, B0](b), Some(table), true).normalize
  }
}