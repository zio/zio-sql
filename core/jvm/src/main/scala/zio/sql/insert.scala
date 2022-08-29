package zio.sql

import zio.schema.Schema
//import zio.sql.macros._

trait InsertModule { self: ExprModule with TableModule with SelectModule with InsertUtilsModule =>

  sealed case class InsertBuilder[F, Source, AllColumnIdentities, B <: SelectionSet[Source], ColsRepr](
    table: Table.Source.Aux_[Source, AllColumnIdentities],
    sources: Selection.Aux[F, Source, B, ColsRepr]
  ) {

    def values[Z](values: Seq[Z])(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source],
      //insertLike: InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]
    ): Insert[Source, Z] = Insert(table, sources.value, values)

    def values[Z](value: Z)(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source],
    //  insertLike: InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]
    ): Insert[Source, Z] = Insert(table, sources.value, Seq(value))
  }

  sealed case class Insert[A, Z](table: Table.Source.Aux[A], sources: SelectionSet[A], values: Seq[Z])(implicit
    schemaN: Schema[Z]
  )

  implicit def convertOptionToSome[A](implicit op: Schema[Option[A]]): Schema[Some[A]] =
    op.transformOrFail[Some[A]](
      {
        case Some(a) => Right(Some(a))
        case None    => Left("cannot encode Right")
      },
      someA => Right(someA)
    )
  implicit val none: Schema[None.type] = Schema.singleton(None)
}
