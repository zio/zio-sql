package zio.sql.delete

import zio.sql.table._
import zio.sql.expr.Expr

final case class Delete[A](table: Table.Aux[A], whereExpr: Expr[_, A, Boolean]) {
  def where[F](expr: Expr[F, A, Boolean]): Delete[A] = Delete(table, expr)
}
