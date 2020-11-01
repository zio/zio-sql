package zio.sql

trait DeleteModule { self: ExprModule with TableModule with SelectModule with TypeTagModule =>

  sealed case class DeleteBuilder[F[_], A, B](table: Table.Aux[A]) {
    def where[F1](expr: Expr[F1, A, Boolean]): Delete[F1, A] = Delete(table, expr)
  }

  sealed case class Delete[F, A](table: Table.Aux[A], whereExpr: Expr[F, A, Boolean])

}
