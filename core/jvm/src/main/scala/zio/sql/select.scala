package zio.sql

import scala.language.implicitConversions

trait SelectModule { self: ExprModule with TableModule with UtilsModule =>

  sealed case class Selector[F, Source, B <: SelectionSet[Source], Unaggregated](selection: Selection[F, Source, B])

  object Selector extends SelectorImplicitLowerPriority {
    implicit def aggregatedSelectorToBuilder[F, Source, B <: SelectionSet[Source]](
      selector: Selector[F, Source, B, Any]
    )(implicit i: Features.IsFullyAggregated[F]): SelectBuilder[F, Source, B] =
      SelectBuilder(selector.selection)

    implicit def notAggregatedSelectorToBuilder[F, Source, B <: SelectionSet[Source], Unaggregated](
      selector: Selector[F, Source, B, Unaggregated]
    )(implicit i: Features.IsNotAggregated[F]): SelectBuilder[F, Source, B] =
      SelectBuilder(selector.selection)
  }

  trait SelectorImplicitLowerPriority {
    implicit def partiallyAggregatedSelectorToBuilder[F, Source, B <: SelectionSet[Source], Unaggregated](
      selector: Selector[F, Source, B, Unaggregated]
    ): AggSelectBuilder[F, Source, B, Unaggregated] =
      AggSelectBuilder[F, Source, B, Unaggregated](selector.selection)

    implicit def noTable[F, Source >: Any, B <: SelectionSet[Source]](
      selectBuilder: Selector[F, Source, B, Any]
    )(implicit
      ev: B <:< SelectionSet.Cons[
        Source,
        selectBuilder.selection.value.ColumnHead,
        selectBuilder.selection.value.SelectionTail
      ],
      normalizer: TrailingUnitNormalizer[selectBuilder.selection.value.ResultTypeRepr]
    ): Read.Select[
      F,
      normalizer.Out,
      Source,
      selectBuilder.selection.value.ColumnHead,
      selectBuilder.selection.value.SelectionTail
    ] = {
      type B0 = SelectionSet.ConsAux[
        selectBuilder.selection.value.ResultTypeRepr,
        Source,
        selectBuilder.selection.value.ColumnHead,
        selectBuilder.selection.value.SelectionTail
      ]
      val b: B0 = selectBuilder.selection.value.asInstanceOf[B0]

      Read.Subselect(Selection[F, Source, B0](b), None, true).normalize
    }
  }

  sealed case class AggSelectBuilder[F0, Source, B <: SelectionSet[Source], Unaggregated](
    selection: Selection[F0, Source, B]
  ) {

    def from[Source0 <: Source](table: Table.Aux[Source0])(implicit
      ev: B <:< SelectionSet.Cons[Source0, selection.value.ColumnHead, selection.value.SelectionTail],
      normalizer: TrailingUnitNormalizer[selection.value.ResultTypeRepr]
    ): AggSelectBuilderGroupBy[
      F0,
      normalizer.Out,
      Source0,
      selection.value.ColumnHead,
      selection.value.SelectionTail,
      Unaggregated
    ] = {
      type B0 = SelectionSet.ConsAux[
        selection.value.ResultTypeRepr,
        Source0,
        selection.value.ColumnHead,
        selection.value.SelectionTail
      ]
      val b: B0 = selection.value.asInstanceOf[B0]

      AggSelectBuilderGroupBy[
        F0,
        normalizer.Out,
        Source0,
        selection.value.ColumnHead,
        selection.value.SelectionTail,
        Unaggregated
      ](Read.Subselect(Selection[F0, Source0, B0](b), Some(table), true).normalize)
    }
  }

  sealed case class AggSelectBuilderGroupBy[F, Repr, Source, Head, Tail <: SelectionSet[Source], Unaggregated](
    select: Read.Select[F, Repr, Source, Head, Tail]
  ) {
    import Read.ExprSet._

    // format: off
    def groupBy[F1, B](expr: Expr[F1, Source, B])(
        implicit ev1: F1 =:= Unaggregated
      ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr)
      
    def groupBy[F1, F2](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any])(
      implicit ev1: F1 with F2 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2)

    def groupBy[F1, F2, F3](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any])(
      implicit ev1: F1 with F2 with F3 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3)

    def groupBy[F1, F2, F3, F4](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4)

    def groupBy[F1, F2, F3, F4, F5](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5)

    def groupBy[F1, F2, F3, F4, F5, F6](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6)

    def groupBy[F1, F2, F3, F4, F5, F6, F7](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F10, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F10, Source, Any], expr11: Expr[F11, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any], expr21: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21<:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21)

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any], expr21: Expr[F9, Source, Any], expr22: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22 <:< Unaggregated
    ): Read.Select[F, Repr, Source, Head, Tail] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22)
    // format: on
  }

  sealed case class SelectBuilder[F0, Source, B <: SelectionSet[Source]](selection: Selection[F0, Source, B]) {

    def from[Source0 <: Source](table: Table.Aux[Source0])(implicit
      ev: B <:< SelectionSet.Cons[Source0, selection.value.ColumnHead, selection.value.SelectionTail],
      normalizer: TrailingUnitNormalizer[selection.value.ResultTypeRepr]
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

  final class SubselectPartiallyApplied[ParentTable] {
    def apply[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SubselectBuilder[F, A, B, ParentTable] =
      SubselectBuilder(selection)
  }

  sealed case class SubselectBuilder[F, Source, B <: SelectionSet[Source], ParentTable](
    selection: Selection[F, Source, B]
  ) {
    def from[Source0](table: Table.Aux[Source0])(implicit
      ev1: Source0 with ParentTable <:< Source,
      ev2: B <:< SelectionSet.Cons[Source, selection.value.ColumnHead, selection.value.SelectionTail],
      normalizer: TrailingUnitNormalizer[selection.value.ResultTypeRepr]
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

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+Out] { self =>
    type ResultType

    val mapper: ResultType => Out

    type ColumnHead
    type HeadIdentity
    type ColumnTail <: ColumnSet

    type CS <: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

    val columnSet: CS

    /**
     * Maps the [[Read]] query's output to another type by providing a function
     * that can transform from the current type to the new type.
     */
    def map[Out2](f: Out => Out2): Read.Aux[ResultType, Out2] =
      Read.Mapped(self, f)

    def asTable(name: TableName): Table.DerivedTable[Out, Read[Out]]

    def to[Target](f: Out => Target): Read[Target] =
      self.map { resultType =>
        f(resultType)
      }

    def union[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
      Read.Union[ResultType, Out1](self, that, true)

    def unionAll[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
      Read.Union[ResultType, Out1](self, that, false)
  }

  object Read {
    sealed trait ExprSet[-Source] {
      type Append[F2, Source1 <: Source, B2] <: ExprSet[Source1]
      def ++[F2, Source1 <: Source, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2]
    }

    object ExprSet {
      type NoExpr = NoExpr.type
      case object NoExpr extends ExprSet[Any] {
        override type Append[F2, Source1 <: Any, B2] = ExprCons[F2, Source1, B2, NoExpr]
        override def ++[F2, Source1 <: Any, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2] =
          ExprCons(that, NoExpr)
      }

      sealed case class ExprCons[F, Source, B, T <: ExprSet[Source]](head: Expr[F, Source, B], tail: T)
          extends ExprSet[Source] {
        override type Append[F2, Source1 <: Source, B2] =
          ExprCons[F, Source1, B, tail.Append[F2, Source1, B2]]
        override def ++[F2, Source1 <: Source, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2] =
          ExprCons(head, tail.++[F2, Source1, B2](that))
      }
    }

    type Aux[ResultType0, Out] = Read[Out] {
      type ResultType = ResultType0
    }

    sealed case class Mapped[Repr, Out, Out2](read: Read.Aux[Repr, Out], f: Out => Out2) extends Read[Out2] { self =>
      override type ResultType = Repr

      override val mapper = read.mapper.andThen(f)

      override type ColumnHead = read.ColumnHead
      override type ColumnTail = read.ColumnTail
      override type CS         = read.CS

      override type HeadIdentity = read.HeadIdentity

      override val columnSet: CS = read.columnSet

      override def asTable(name: TableName): Table.DerivedTable[Out2, Mapped[Repr, Out, Out2]] =
        Table.DerivedTable[Out2, Mapped[Repr, Out, Out2]](self, name)
    }

    type Select[F, Repr, Source, Head, Tail <: SelectionSet[Source]] = Subselect[F, Repr, Source, Source, Head, Tail]

    sealed case class Subselect[F, Repr, Source, Subsource, Head, Tail <: SelectionSet[Source]](
      selection: Selection[F, Source, SelectionSet.ConsAux[Repr, Source, Head, Tail]],
      table: Option[Table.Aux[Subsource]],
      whereExpr: Expr[_, Source, Boolean],
      groupByExprs: ExprSet[Source] = ExprSet.NoExpr,
      havingExpr: Expr[_, Source, Boolean] = true,
      orderByExprs: List[Ordering[Expr[_, Source, Any]]] = Nil,
      offset: Option[Long] = None, //todo don't know how to do this outside of postgres/mysql
      limit: Option[Long] = None
    ) extends Read[Repr] { self =>

      /**
       * The follwing expr would not compile in where clause with F2: Features.IsNotAggregated
       *
       *        List(minStationsQuery, maxStationsQuery)
       *            .flatten
       *            .reduceLeftOption[Expr[_, metroLine.TableType, Boolean]](_ && _)
       *            .get
       *
       * TODO try to make phantom type F2 composable
       */
      def where[F2](
        whereExpr2: Expr[F2, Source, Boolean]
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Subselect[F, Repr, Source, Subsource, Head, Tail] = copy(limit = Some(n))

      def offset(n: Long): Subselect[F, Repr, Source, Subsource, Head, Tail] = copy(offset = Some(n))

      def orderBy(
        o: Ordering[Expr[_, Source, Any]],
        os: Ordering[Expr[_, Source, Any]]*
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(orderByExprs = self.orderByExprs ++ (o :: os.toList))

      /**
       * TODO find a way to make following not compile -> fkCustomerId need to be `groupped by`
       *           select(fkCustomerId)
       *             .from(orders)
       *             .having(Count(orderId) > 4)
       */
      def having[F2: Features.IsFullyAggregated](
        havingExpr2: Expr[F2, Source, Boolean]
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(havingExpr = self.havingExpr && havingExpr2)

      /**
       * TODO restrict _ : IsNotAggregated (hopefully without 22 boilerplate overrides)
       * cannot move it toAggBuilder because select(fkCustomerId).from(orders) is valid sql)
       */
      def groupBy(
        key: Expr[_, Source, Any],
        keys: Expr[_, Source, Any]*
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(groupByExprs =
          (key :: keys.toList).foldLeft[ExprSet[Source]](ExprSet.NoExpr)((tail, head) => ExprSet.ExprCons(head, tail))
        )

      def normalize(implicit
        instance: TrailingUnitNormalizer[ResultType]
      ): Subselect[F, instance.Out, Source, Subsource, Head, Tail] =
        self.asInstanceOf[Subselect[F, instance.Out, Source, Subsource, Head, Tail]]

      override def asTable(
        name: TableName
      ): Table.DerivedTable[Repr, Subselect[F, Repr, Source, Subsource, Head, Tail]] =
        Table.DerivedTable[Repr, Subselect[F, Repr, Source, Subsource, Head, Tail]](self, name)

      override type ResultType = Repr

      override val mapper: Repr => Repr = identity(_)

      override type ColumnHead = selection.value.ColumnHead
      override type ColumnTail = selection.value.ColumnTail

      override type HeadIdentity = selection.value.HeadIdentity

      override val columnSet: CS = selection.value.columnSet

      override type CS = selection.value.CS
    }

    object Subselect {
      implicit def subselectToExpr[F <: Features.Aggregated[_], Repr, Source, Subsource, Head](
        subselect: Read.Subselect[F, Repr, _ <: Source, Subsource, Head, SelectionSet.Empty]
      ): Expr[Features.Derived, Any, Head] =
        Expr.Subselect(subselect)
    }

    sealed case class Union[Repr, Out](left: Read.Aux[Repr, Out], right: Read.Aux[Repr, Out], distinct: Boolean)
        extends Read[Out] { self =>
      override type ResultType = Repr

      override val mapper: ResultType => Out = left.mapper

      override type ColumnHead = left.ColumnHead
      override type ColumnTail = left.ColumnTail

      override type HeadIdentity = left.HeadIdentity

      override type CS = left.CS

      override val columnSet: CS = left.columnSet

      override def asTable(name: TableName): Table.DerivedTable[Out, Union[Repr, Out]] =
        Table.DerivedTable[Out, Union[Repr, Out]](self, name)
    }

    // TODO add name to literal selection - e.g. select '1' as one
    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[(B, Unit)] { self =>
      override type ResultType = (B, Unit)

      override val mapper: ResultType => (B, Unit) = identity(_)

      def typeTag: TypeTag[B] = implicitly[TypeTag[B]]

      override type ColumnHead = B
      override type ColumnTail = ColumnSet.Empty

      override type CS = ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

      override val columnSet: CS = ColumnSet.Cons(Column.Indexed[ColumnHead, HeadIdentity](), ColumnSet.Empty)

      override def asTable(name: TableName): Table.DerivedTable[(B, Unit), Literal[B]] =
        Table.DerivedTable[(B, Unit), Literal[B]](self, name)
    }

    def lit[B: TypeTag](values: B*): Read[(B, Unit)] = Literal(values.toSeq)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[F, -A, +B <: SelectionSet[A]](value: B) { self =>

    type ColsRepr = value.ResultTypeRepr

    def ++[F2, A1 <: A, C <: SelectionSet[A1]](
      that: Selection[F2, A1, C]
    ): Selection[F :||: F2, A1, self.value.Append[A1, C]] =
      Selection(self.value ++ that.value)
  }

  object Selection {
    import ColumnSelection._
    import SelectionSet.{ Cons, Empty }

    type Aux[F, -A, +B <: SelectionSet[A], ColsRepr0] = Selection[F, A, B] {
      type ColsRepr = ColsRepr0
    }

    def constantOption[A: TypeTag](value: A, option: Option[ColumnName]): Selection[Any, Any, Cons[Any, A, Empty]] =
      Selection(Cons(Constant(value, option), Empty))

    def constant[A: TypeTag](value: A): Selection[Any, Any, Cons[Any, A, Empty]] = constantOption(value, None)

    def constantAs[A: TypeTag](value: A, name: ColumnName): Selection[Any, Any, Cons[Any, A, Empty]] =
      constantOption(value, Some(name))

    def computedOption[F, A, B](expr: Expr[F, A, B], name: Option[ColumnName]): Selection[F, A, Cons[A, B, Empty]] =
      Selection(Cons(Computed(expr, name), Empty))

    def computed[F, A, B](expr: Expr[F, A, B]): Selection[F, A, Cons[A, B, Empty]] =
      computedOption(expr, None)

    def computedAs[F, A, B](expr: Expr[F, A, B], name: ColumnName): Selection[F, A, Cons[A, B, Empty]] =
      computedOption(expr, Some(name))
  }

  sealed trait ColumnSelection[-Source, +ColumnType] {
    type ColumnType0 <: ColumnType

    def name: Option[ColumnName]

    val toColumn: Column[ColumnType]
  }

  object ColumnSelection {
    sealed case class Constant[ColumnType: TypeTag](value: ColumnType, name: Option[ColumnName])
        extends ColumnSelection[Any, ColumnType] {
      def typeTag: TypeTag[ColumnType] = implicitly[TypeTag[ColumnType]]

      val toColumn: Column[ColumnType] = name match {
        case Some(value) => Column.Named(value)
        case None        => Column.Indexed()
      }
    }

    sealed case class Computed[F, Source, ColumnType](expr: Expr[F, Source, ColumnType], name: Option[ColumnName])
        extends ColumnSelection[Source, ColumnType] {
      implicit def typeTag: TypeTag[ColumnType] = Expr.typeTagOf(expr)

      val toColumn: Column[ColumnType] = name match {
        case Some(value) => Column.Named(value)
        case None        => Column.Indexed()
      }
    }
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[Source1, T]

    type ResultTypeRepr

    type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    type ColumnHead
    type ColumnTail <: ColumnSet
    type SelectionTail <: SelectionSet[Source]
    type HeadIdentity

    type CS <: ColumnSet
    def columnSet: CS

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
      override type ColumnTail    = ColumnSet.Empty
      override type SelectionTail = SelectionSet.Empty

      override type HeadIdentity = Any

      override type CS = ColumnSet.Empty
      override def columnSet: CS = ColumnSet.Empty

      override type SelectionsRepr[Source1, T] = Unit

      override type ResultTypeRepr = Unit

      override type Append[Source1, That <: SelectionSet[Source1]] = That

      override def ++[Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      override def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
    }

    sealed case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
        extends SelectionSet[Source] { self =>

      override type ColumnHead    = A
      override type ColumnTail    = tail.CS
      override type SelectionTail = B

      override type HeadIdentity = head.toColumn.Identity

      override type CS = ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

      override def columnSet: CS =
        ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity](head.toColumn, tail.columnSet)

      override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

      override type ResultTypeRepr = (A, tail.ResultTypeRepr)

      override type Append[Source1, That <: SelectionSet[Source1]] =
        Cons[Source1, A, tail.Append[Source1, That]]

      override def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      override def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
    }
  }

  sealed trait Ordering[+A] {
    val value: A
  }

  object Ordering {
    sealed case class Asc[A](value: A)  extends Ordering[A]
    sealed case class Desc[A](value: A) extends Ordering[A]

    implicit def exprToOrdering[F, A, B](expr: Expr[F, A, B]): Ordering[Expr[F, A, B]] =
      Asc(expr)
  }

  sealed trait DecodingError extends Exception {
    def message: String
  }

  object DecodingError {
    sealed case class UnexpectedNull(column: Int)                       extends DecodingError {
      def message = s"Expected column with index ${column} to be non-null"
    }
    sealed case class UnexpectedType(expected: TypeTag[_], actual: Int) extends DecodingError {
      def message = s"Expected type ${expected} but found ${actual}"
    }
    sealed case class MissingColumn(column: Int)                        extends DecodingError {
      def message = s"The column with index ${column} does not exist"
    }
    case object Closed                                                  extends DecodingError {
      def message = s"The ResultSet has been closed, so decoding is impossible"
    }
  }
}
