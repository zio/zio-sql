package zio.sql.insert

import zio.schema.Schema
import zio.sql.macros._
import zio.sql.table._
import zio.sql.select._

final case class InsertBuilder[F, Source, AllColumnIdentities, B <: SelectionSet[Source], ColsRepr](
  table: Table.Source.Aux_[Source, AllColumnIdentities],
  sources: Selection.Aux[F, Source, B, ColsRepr]
) {

  def values[Z](values: Seq[Z])(implicit
    schemaCC: Schema[Z],
    schemaValidity: InsertLike[F, ColsRepr, AllColumnIdentities, Z]
  ): Insert[Source, Z] = Insert(table, sources.value, values)

  def values[Z](value: Z)(implicit
    schemaCC: Schema[Z],
    schemaValidity: InsertLike[F, ColsRepr, AllColumnIdentities, Z]
  ): Insert[Source, Z] = Insert(table, sources.value, Seq(value))
}
