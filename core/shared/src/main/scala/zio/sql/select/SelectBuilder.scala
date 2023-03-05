package zio.sql.select

import zio.sql.table._
import zio.sql.macros._
import scala.language.implicitConversions

final case class SelectBuilder[F0, Source, B <: SelectionSet[Source]](selection: Selection[F0, Source, B]) {

  def from[Source0 <: Source](table: Table.Aux[Source0])(implicit
    ev: B <:< SelectionSet.Cons[Source0, selection.value.ColumnHead, selection.value.SelectionTail],
    normalizer: Normalizer[selection.value.ResultTypeRepr]
  ): Read.Select[
    F0,
    normalizer.Out,
    Source0,
    selection.value.ColumnHead,
    selection.value.SelectionTail
  ] = {
    type B0 = SelectionSet.ConsAux[
      selection.value.ResultTypeRepr,
      Source0,
      selection.value.ColumnHead,
      selection.value.SelectionTail
    ]
    val b: B0 = selection.value.asInstanceOf[B0]

    Read.Subselect(Selection[F0, Source0, B0](b), Some(table), true).normalize
  }
}

object SelectBuilder {

  implicit def noTable[F, Source >: Any, B <: SelectionSet[Source]](
    builder: SelectBuilder[F, Source, B]
  )(implicit
    ev: B <:< SelectionSet.Cons[
      Source,
      builder.selection.value.ColumnHead,
      builder.selection.value.SelectionTail
    ],
    normalizer: Normalizer[builder.selection.value.ResultTypeRepr]
  ): Read.Select[
    F,
    normalizer.Out,
    Source,
    builder.selection.value.ColumnHead,
    builder.selection.value.SelectionTail
  ] = {
    type B0 = SelectionSet.ConsAux[
      builder.selection.value.ResultTypeRepr,
      Source,
      builder.selection.value.ColumnHead,
      builder.selection.value.SelectionTail
    ]
    val b: B0 = builder.selection.value.asInstanceOf[B0]

    Read.Subselect(Selection[F, Source, B0](b), None, true).normalize
  }
}
