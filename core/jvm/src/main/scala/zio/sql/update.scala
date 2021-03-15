package zio.sql

trait UpdateModule { self: ExprModule with TableModule with SelectModule =>

  sealed case class UpdateBuilder[A](table: Table.Aux[A]) {
    def set[F: Features.IsSource, Value: TypeTag](lhs: Expr[F, A, Value], rhs: Expr[_, A, Value]): Update[A] =
      Update(table, Set(lhs, rhs) :: Nil, true)
  }

  // UPDATE table
  // SET foo = bar
  // WHERE baz > buzz
  //todo `set` must be non-empty
  sealed case class Update[A](table: Table.Aux[A], set: List[Set[_, A]], whereExpr: Expr[_, A, Boolean]) {

    def set[F: Features.IsSource, Value: TypeTag](lhs: Expr[F, A, Value], rhs: Expr[_, A, Value]): Update[A] =
      copy(set = set :+ Set(lhs, rhs))

    def where(whereExpr2: Expr[_, A, Boolean]): Update[A] =
      copy(whereExpr = whereExpr && whereExpr2)

  }
}
