package zio.sql.select

import zio.sql.expr.Expr
import scala.language.experimental.macros

final case class SelectByCommaBuilder() {
  def apply[F, Source, Set <: SelectionSet[Source]](exprs: Expr[F, Source, _]*): SelectBuilder[F, Source, Set] =
    macro SelectionMacro.selectApplyMacro[F, Source]
}