package zio.sql.update

import zio.sql.table._
import zio.sql.typetag._
import zio.sql.Features

import zio.sql.expr.{Expr, Set}

// UPDATE table
// SET foo = bar
// WHERE baz > buzz
// todo `set` must be non-empty
final case class Update[A](table: Table.Aux[A], set: List[Set[_, A]], whereExpr: Expr[_, A, Boolean]) {

  def set[F: Features.IsSource, Value: TypeTag](lhs: Expr[F, A, Value], rhs: Expr[_, A, Value]): Update[A] =
    copy(set = set :+ Set(lhs, rhs))

  def where(whereExpr2: Expr[_, A, Boolean]): Update[A] =
    copy(whereExpr = whereExpr && whereExpr2)
}
