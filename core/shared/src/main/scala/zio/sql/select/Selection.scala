package zio.sql.select

import zio.sql.typetag.TypeTag
import zio.sql.expr.Expr

/**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
final case class Selection[F, -A, +B <: SelectionSet[A]](value: B) { self =>

  type ColsRepr = value.ResultTypeRepr

  type ColumnsOut[S] = value.ColumnsOut[S]

  def columns[S](name: String): ColumnsOut[S] = value.columns[S](name)

  def ++[F2, A1 <: A, C <: SelectionSet[A1]](
    that: Selection[F2, A1, C]
  ): Selection[F with F2, A1, self.value.Append[A1, C]] =
    Selection(self.value ++ that.value)
}

object Selection {
  import ColumnSelection._
  import SelectionSet.{ Cons, Empty }

  type Aux[F, -A, +B <: SelectionSet[A], ColsRepr0] = Selection[F, A, B] {
    type ColsRepr = ColsRepr0
  }

  def constantOption[A: TypeTag](value: A, option: Option[String]): Selection[Any, Any, Cons[Any, A, Empty]] =
    Selection(Cons(Constant(value, option), Empty))

  def constant[A: TypeTag](value: A): Selection[Any, Any, Cons[Any, A, Empty]] = constantOption(value, None)

  def constantAs[A: TypeTag](value: A, name: String): Selection[Any, Any, Cons[Any, A, Empty]] =
    constantOption(value, Some(name))

  def computedOption[F, A, B](expr: Expr[F, A, B], name: Option[String]): Selection[F, A, Cons[A, B, Empty]] =
    Selection(Cons(Computed(expr, name), Empty))

  def computed[F, A, B](expr: Expr[F, A, B]): Selection[F, A, Cons[A, B, Empty]] =
    computedOption(expr, None)

  def computedAs[F, A, B](expr: Expr[F, A, B], name: String): Selection[F, A, Cons[A, B, Empty]] =
    computedOption(expr, Some(name))
}
