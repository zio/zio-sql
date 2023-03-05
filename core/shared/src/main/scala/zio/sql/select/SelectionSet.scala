package zio.sql.select

import zio.sql.expr.Expr
import zio.sql.Features
import zio.sql.table.Column

sealed trait SelectionSet[-Source] {
  type SelectionsRepr[Source1, T]

  type ResultTypeRepr

  type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

  type ColumnHead

  type SelectionTail <: SelectionSet[Source]
  type HeadIdentity

  type ColumnsOut[S]

  def columns[S](name: String): ColumnsOut[S]

  def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

  def selectionsUntyped: List[ColumnSelection[Source, _]]

  def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
}

object SelectionSet {

  type Aux[-Source, ResultTypeRepr0] =
    SelectionSet[Source] {
      type ResultTypeRepr = ResultTypeRepr0
    }

  type ConsAux[ResultTypeRepr0, -Source, A, B <: SelectionSet[Source]] =
    SelectionSet.Cons[Source, A, B] {
      type ResultTypeRepr = ResultTypeRepr0
    }

  type Empty = Empty.type

  case object Empty extends SelectionSet[Any] {

    override type ColumnHead    = Unit
    override type SelectionTail = SelectionSet.Empty

    override type HeadIdentity = Any

    override type SelectionsRepr[Source1, T] = Unit

    override type ResultTypeRepr = Unit

    override type Append[Source1, That <: SelectionSet[Source1]] = That

    override type ColumnsOut[S] = Unit

    override def columns[S](name: String): ColumnsOut[S] = ()

    override def ++[Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
      that

    override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

    override def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
  }

  final case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
      extends SelectionSet[Source] { self =>

    override type ColumnHead    = A
    override type SelectionTail = B

    override type HeadIdentity = head.toColumn.Identity

    override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

    override type ResultTypeRepr = (A, tail.ResultTypeRepr)

    override type Append[Source1, That <: SelectionSet[Source1]] =
      Cons[Source1, A, tail.Append[Source1, That]]

    override type ColumnsOut[S] = (Expr[Features.Source[HeadIdentity, S], S, A], tail.ColumnsOut[S])

    override def columns[S](name: String): ColumnsOut[S] = {
      val column: Column.Aux[A, HeadIdentity] = head.toColumn

      (Expr.Source(name, column), tail.columns[S](name))
    }

    override def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
      Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

    override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

    override def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
  }
}
