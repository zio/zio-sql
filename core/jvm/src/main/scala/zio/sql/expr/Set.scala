package zio.sql.expr

import zio.sql.typetag.TypeTag
import zio.sql.Features
import com.github.ghik.silencer.silent

sealed trait Set[F, -A] {
  type Value

  def lhs: Expr[F, A, Value]
  def rhs: Expr[_, A, Value]

  def typeTag: TypeTag[Value]

}

object Set {
  type Aux[F, -A, Value0] = Set[F, A] { type Value = Value0 }

  @silent
  def apply[F: Features.IsSource, A, Value0: TypeTag](
    lhs0: Expr[F, A, Value0],
    rhs0: Expr[_, A, Value0]
  ): Set.Aux[F, A, Value0] =
    new Set[F, A] {
      type Value = Value0

      def lhs = lhs0
      def rhs = rhs0

      def typeTag = implicitly[TypeTag[Value]]
    }
}
