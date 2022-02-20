package zio.sql

/**
 * User lands in this builder in case there is a "partial aggregation" in a query like:
 *
 * select Count(id), customer_id
 *   from orders
 *   group by customer_id
 *
 * In such a case its not possible to execute query before we group by customer_id.
 * As we need to check F of every `Expr`, `groupBy` method is overloaded by 22 arities - even though partically, mostly only few are necessary.
 *
 * Without group by clause, `AggSelectBuilderGroupBy` is not executable so calling `groupBy` is the only choice.
 * The query won't compile unless user groups at least by required columns.
 *
 * In case we are not dealing with partial aggregation, this builder is skipped and user can groupBy in Read.Subselect as usual.
 * Following are all valid examples of queries not using this `AggSelectBuilderGroupBy` trait.
 *
 *  select Count(id)
 *     from orders
 *     group by customer_id
 *
 *  select Count(id)
 *     from orders
 *
 *  select customer_id
 *     from orders
 *    group by customer_id
 *
 *  select customer_id
 *     from orders
 *     group by customer_id
 */
trait GroupByUtilsModule { self: SelectModule with ExprModule =>

  sealed case class AggSelectBuilderGroupBy[F, Repr, Source, Head, Tail <: SelectionSet[Source], Unaggregated](
    select: Read.Select[F, Repr, Source, Head, Tail]
  ) {
    import Read.ExprSet._

    // format: off
    def groupBy[F1, B](expr: Expr[F1, Source, B])(
        implicit ev1: F1 =:= Unaggregated
      ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1] = 
      select.copy(groupByExprs = NoExpr ++ expr).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1]]
      
    def groupBy[F1, F2](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any])(
      implicit ev1: F1 with F2 <:< Unaggregated
    ):  Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2]]

    def groupBy[F1, F2, F3](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any])(
      implicit ev1: F1 with F2 with F3 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3]]

    def groupBy[F1, F2, F3, F4](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4]]

    def groupBy[F1, F2, F3, F4, F5](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5]]

    def groupBy[F1, F2, F3, F4, F5, F6](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F10, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F10, Source, Any], expr11: Expr[F11, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any], expr21: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21<:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21]]

    def groupBy[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22](expr: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any], expr4: Expr[F4, Source, Any], expr5: Expr[F5, Source, Any], expr6: Expr[F6, Source, Any], expr7: Expr[F7, Source, Any], expr8: Expr[F8, Source, Any], expr9: Expr[F9, Source, Any], expr10: Expr[F9, Source, Any], expr11: Expr[F9, Source, Any], expr12: Expr[F9, Source, Any], expr13: Expr[F9, Source, Any], expr14: Expr[F9, Source, Any], expr15: Expr[F9, Source, Any], expr16: Expr[F9, Source, Any], expr17: Expr[F9, Source, Any], expr18: Expr[F9, Source, Any], expr19: Expr[F9, Source, Any], expr20: Expr[F9, Source, Any], expr21: Expr[F9, Source, Any], expr22: Expr[F9, Source, Any])(
      implicit ev1: F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22 <:< Unaggregated
    ): Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22] = 
      select.copy(groupByExprs = NoExpr ++ expr ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22).asInstanceOf[Read.Subselect.WithGroupByF[F, Repr, Source, Source, Head, Tail, F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22]]
    // format: on
  }
}
