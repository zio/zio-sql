package zio.sql.select

import zio.sql.expr.Expr
import zio.sql.select._
import scala.language.experimental.macros


final case class SubselectByCommaBuilder[ParentTable]() {
  def apply[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) =
    SubselectBuilder[F, A, B, ParentTable](selection)

  def apply[F, Source, Set <: SelectionSet[Source]](exprs: Expr[F, Source, _]*): SubselectBuilder[F, Source, Set, ParentTable] =
      macro SelectionMacro.subselectApplyMacro[F, Source, ParentTable]
}
