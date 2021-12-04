package zio.sql

import zio.schema.Schema
import scala.language.implicitConversions

/**
 * insert into
 *     customers
 *        (id, first_name, last_name, verified, dob)
 * values
 *        ('0511474d-8eed-4307-bdb0-e39a561205b6', 'Jaro', 'Regec, true' 1999-11-02)
 *
 * final case class Customer(id: UUID, fName: String, lName: Strng, verified: Boolean, dob: LocalDate)
 *
 * implicit val Customer = DeriveSchema.gen[Customer]
 * val customerValues : List[Customer] = ???
 *
 * insertInto(customers)(customerId +++ fName +++ lName +++ verified +++ dob)
 *    values(customerValues)
 *
 * TODO
 * 1. change SourceSet +++ to :: or something better - cannot use ++ because its selectionSet then, && exists on expr
 * 2. make columns null, not null aware
 * 3. automatically generated columns (postgres idenitity) do not accept insert
 * 4. make better error messages
 * 5. try to generate DSL tables at compile type from sql file with compiler plugin
 * 6. translate new inserts to sql query
 * 7. how to support tables with more than 22 columns ?
 */
trait InsertModule { self: ExprModule with TableModule =>

  sealed case class InsertBuilder[Source, SourceTypes, ColsRepr](
    table: Table.Source.Aux[Source],
    sources: SourceSet.Aux[Source, SourceTypes, ColsRepr]
  ) {

    def values[Z](values: Seq[Z])(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[Z, ColsRepr]
    ): Insert[Source, Z] = Insert(table, sources, values)
  }

  sealed case class Insert[A, N](table: Table.Source.Aux[A], sources: SourceSet[A], values: Seq[N])(implicit
    schemaN: Schema[N]
  )

  sealed trait SourceSet[-Source] { self =>

    type Append[Source1, Appended, ThatIdentity] <: SourceSet[Source1]

    type Repr

    type ColumnTypes

    type Size <: ColumnCount

    def +++[Source1 <: Source, Appended, ThatIdentity](
      that: Expr[Features.Source[ThatIdentity], Source1, Appended]
    ): Append[Source1, Appended, ThatIdentity]
  }

  object SourceSet {

    type Aux[Source0, ColumnTypes0, ColsRepr] = SourceSet[Source0] {
      type ColumnTypes = ColumnTypes0

      type Repr = ColsRepr
    }

    type Empty = Empty.type
    import ColumnCount._

    case object Empty extends SourceSet[Any] {
      override type Size = _0

      override type ColumnTypes = Any

      override type Repr = Unit

      override type Append[Source1, Appended, ThatIdentity] =
        SourceSet.Cons[Source1, Appended, SourceSet.Empty, ThatIdentity]

      override def +++[Source1 <: Any, Appended, ThatIdentity](
        that: Expr[Features.Source[ThatIdentity], Source1, Appended]
      ): Append[Source1, Appended, ThatIdentity] =
        SourceSet.Cons(that, SourceSet.Empty)
    }

    sealed case class Cons[-Source, H, T <: SourceSet[Source], HeadIdentity](
      head: Expr[Features.Source[HeadIdentity], Source, H],
      tail: T
    ) extends SourceSet[Source] { self =>

      override type Size = Succ[tail.Size]

      override type Repr = (H, tail.Repr)

      override type ColumnTypes = HeadIdentity with tail.ColumnTypes

      override type Append[Source1, Appended, ThatIdentity] =
        SourceSet.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity]

      override def +++[Source1 <: Source, Appended, ThatIdentity](
        that: Expr[Features.Source[ThatIdentity], Source1, Appended]
      ): SourceSet.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity] =
        SourceSet.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity](
          head,
          tail.+++(that)
        )
    }
  }

  implicit def exprToSourceSet[Source, H: TypeTag, HeadIdentity](
    head: Expr[Features.Source[HeadIdentity], Source, H]
  ): SourceSet.Cons[Source, H, SourceSet.Empty, HeadIdentity] =
    SourceSet.Cons(head, SourceSet.Empty)

  sealed trait SchemaValidity[Z, ColsRepr]

  object SchemaValidity {
    implicit def caseClass1[A, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass1[A, Z],
      ev: ColsRepr <:< (A, Unit)
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass2[A1, A2, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass2[A1, A2, Z],
      ev: ColsRepr <:< (A1, (A2, Unit))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass3[A1, A2, A3, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass3[A1, A2, A3, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, Unit)))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass4[A1, A2, A3, A4, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass4[A1, A2, A3, A4, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, Unit))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass5[A1, A2, A3, A4, A5, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, Unit)))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass6[A1, A2, A3, A4, A5, A6, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, Unit))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass7[A1, A2, A3, A4, A5, A6, A7, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, Unit)))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, Unit))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, Unit)))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, Unit))))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, Unit)))))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, Unit))))))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, Unit)))))))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, Unit))))))))))))))
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z, ColsRepr](implicit
      ccSchema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, Unit))))))))))))))
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z, ColsRepr](
      implicit
      ccSchema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, Unit)))))))))))))))
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z, ColsRepr](
      implicit
      ccSchema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, Unit))))))))))))))))
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass18[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      Z,
      ColsRepr
    ](implicit
      ccSchema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, Unit))))))))))))))))
        )
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass19[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      Z,
      ColsRepr
    ](implicit
      ccSchema: Schema.CaseClass19[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        Z
      ],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (
            A3,
            (
              A4,
              (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, Unit)))))))))))))))
            )
          )
        )
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass20[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      Z,
      ColsRepr
    ](implicit
      ccSchema: Schema.CaseClass20[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        Z
      ],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (
            A3,
            (
              A4,
              (
                A5,
                (
                  A6,
                  (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, Unit))))))))))))))
                )
              )
            )
          )
        )
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass21[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      Z,
      ColsRepr
    ](implicit
      ccSchema: Schema.CaseClass21[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        Z
      ],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (
            A3,
            (
              A4,
              (
                A5,
                (
                  A6,
                  (
                    A7,
                    (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, Unit))))))))))))))
                  )
                )
              )
            )
          )
        )
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}

    implicit def caseClass22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22,
      Z,
      ColsRepr
    ](implicit
      ccSchema: Schema.CaseClass22[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22,
        Z
      ],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (
            A3,
            (
              A4,
              (
                A5,
                (
                  A6,
                  (
                    A7,
                    (
                      A8,
                      (
                        A9,
                        (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, Unit)))))))))))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ): SchemaValidity[Z, ColsRepr] =
      new SchemaValidity[Z, ColsRepr] {}
  }
}
