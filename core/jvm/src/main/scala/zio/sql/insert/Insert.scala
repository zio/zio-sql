package zio.sql.insert

import zio.schema.Schema
import zio.sql.table._
import zio.sql.select._

final case class Insert[A, Z](table: Table.Source.Aux[A], sources: SelectionSet[A], values: Seq[Z])(implicit
  schemaN: Schema[Z]
)