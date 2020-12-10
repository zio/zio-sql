package zio.sql

trait DeleteModule { self: ExprModule with TableModule with SelectModule =>

  sealed case class Delete[A](table: Table.Aux[A], whereExpr: Expr[_, A, Boolean]) {
    def where[F](expr: Expr[F, A, Boolean]): Delete[A] = Delete(table, expr)
  }
}
