package zio.sql

trait DeleteModule { self: ExprModule with TableModule =>

  sealed case class DeleteBuilder[A](table: Table.Aux[A]) {
    def where[F](expr: Expr[F, A, Boolean]): Delete[F, A] = Delete(table, expr)

    def all[F]: Delete[Features.Literal, A] = Delete(table, Expr.literal(true))
  }

  sealed case class Delete[F, A](table: Table.Aux[A], whereExpr: Expr[F, A, Boolean])

}
