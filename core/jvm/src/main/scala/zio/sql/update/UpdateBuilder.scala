package zio.sql.update

import zio.sql.table._
import zio.sql.typetag._
import zio.sql.Features
import zio.sql.expr._

final case class UpdateBuilder[A](table: Table.Aux[A]) {
  def set[F: Features.IsSource, Value: TypeTag](lhs: Expr[F, A, Value], rhs: Expr[_, A, Value]): Update[A] =
    Update(table, Set(lhs, rhs) :: Nil, true)
}