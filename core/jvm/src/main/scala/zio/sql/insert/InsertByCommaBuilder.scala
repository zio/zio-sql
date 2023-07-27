package zio.sql.insert

import zio.sql.table._
import zio.sql.select._
import scala.language.experimental.macros

final case class InsertByCommaBuilder() {
  def apply[F, Source, Set <: SelectionSet[Source], AllColumnIdentities](
    table: Table.Source.Aux_[Source, AllColumnIdentities]
  )(selections: Selection[F, Source, _ <: SelectionSet[Source]]*): InsertBuilder[F, Source, AllColumnIdentities, Set] =
    macro SelectionMacro.insertApplyMacro[F, Source, AllColumnIdentities]
}
