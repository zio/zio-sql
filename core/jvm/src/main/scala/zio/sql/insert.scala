package zio.sql

import zio.schema.Schema

/**
 * val data = List(
 *  ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Richard', 'Dent, true' 1999-11-02),
 *   ....
 * )
 *
 * insertInto(customers)(customerId +++ fName +++ lName +++ verified +++ dob)
 *    .values(data)
 */
trait InsertModule { self: ExprModule with TableModule with SelectModule with InsertUtilsModule =>

  sealed case class InsertBuilder[F, Source, AllColumnIdentities, B <: SelectionSet[Source], ColsRepr](
    table: Table.Source.Aux_[Source, AllColumnIdentities],
    sources: Selection.Aux[F, Source, B, ColsRepr]
  ) {

    def values[Z](values: Seq[Z])(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source]
    ): Insert[Source, Z] = Insert(table, sources.value, values)

    def values[Z](value: Z)(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source]
    ): Insert[Source, Z] = Insert(table, sources.value, Seq(value))
  }

  sealed case class Insert[A, Z](table: Table.Source.Aux[A], sources: SelectionSet[A], values: Seq[Z])(implicit
    schemaN: Schema[Z]
  )
}
