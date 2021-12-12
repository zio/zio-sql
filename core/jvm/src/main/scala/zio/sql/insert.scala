package zio.sql

import zio.schema.Schema

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
 * 2. make columns null, not null aware
 * 3. automatically generated columns (postgres idenitity) do not accept values by insert
 * 4. make better error messages
 * 5. try to generate DSL tables at compile type from sql file with compiler plugin
 * 6. how to support tables with more than 22 columns ?
 * 7. retrieve generated ID from inserted row
 * 8. explore & add "on conflict do" stuff
 * 9 do we want to keep supporting insertAltInto stuff?
 *
 * TODO
 * 1. support for tuples by inserting
 * 2. add possibility to select and insert source columns separated by comma
 * 3. deal with trailing unit in tuples
 * 4. rollout zio-schema for other APIs ( select, delete, update)
 */

trait InsertModule { self: ExprModule with TableModule with SelectModule =>

  sealed case class InsertBuilder[F, Source, AllColumnIdentities, N, B <: SelectionSet.Aux[Source, ColsRepr], ColsRepr](
    table: Table.Source.AuxN[Source, AllColumnIdentities, N],
    sources: Selection.Aux[F, Source, B, ColsRepr]
  ) {

    def values[Z](values: Seq[Z])(implicit
      schemaCC: Schema[Z],
      schemaValidity: SchemaValidity[F, Z, ColsRepr, AllColumnIdentities]
    ): Insert[Source, Z] = Insert(table, sources.value, values)
  }

  sealed case class Insert[A, N](table: Table.Source.Aux[A], sources: SelectionSet[A], values: Seq[N])(implicit
    schemaN: Schema[N]
  )

  //TODO should be moved to separate file ?
  sealed trait SchemaValidity[F, Z, ColsRepr, AllColumnIdentities]

  // format: off
  object SchemaValidity {

    implicit def caseClass1[F, A, Z, ColsRepr, AllColumnIdentities, Identity1](implicit
      ccSchema: Schema.CaseClass1[A, Z],
      ev: ColsRepr <:< (A, Unit),
      ev2: F <:< Features.Source[Identity1],
      ev3: AllColumnIdentities <:< Identity1
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass2[F, A1, A2, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2](implicit
      ccSchema: Schema.CaseClass2[A1, A2, Z],
      ev: ColsRepr <:< (A1, (A2, Unit)),
      ev2: F <:<  :||:[Features.Source[Identity1], Features.Source[Identity2]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass3[F, A1, A2, A3, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3](implicit
      ccSchema: Schema.CaseClass3[A1, A2, A3, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, Unit))),
      ev2: F <:< Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass4[F, A1, A2, A3, A4, Z, ColsRepr,  AllColumnIdentities, Identity1, Identity2, Identity3, Identity4](implicit
      ccSchema: Schema.CaseClass4[A1, A2, A3, A4, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, Unit)))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass5[F, A1, A2, A3, A4, A5, Z, ColsRepr,  AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5](implicit
      ccSchema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, Unit))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]],  Features.Source[Identity4]],  Features.Source[Identity5]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass6[F, A1, A2, A3, A4, A5, A6, Z, ColsRepr,  AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6](implicit
      ccSchema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, Unit)))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass7[F, A1, A2, A3, A4, A5, A6, A7, Z, ColsRepr,  AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7](implicit
      ccSchema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, Unit))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass8[F, A1, A2, A3, A4, A5, A6, A7, A8, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8](implicit
      ccSchema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, Unit)))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9](implicit
      ccSchema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, Unit))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10](implicit
      ccSchema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, Unit)))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}


    implicit def caseClass11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11](implicit
      ccSchema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, Unit))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12](implicit
      ccSchema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, Unit)))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13](implicit
      ccSchema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, Unit))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], 
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14](implicit
      ccSchema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, Unit)))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15](implicit
      ccSchema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, Unit))))))))))))))
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16](
      implicit
      ccSchema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, Unit)))))))))))))))
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17](
      implicit
      ccSchema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z],
      ev: ColsRepr <:< (
        A1,
        (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, Unit))))))))))))))))
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass18[
      F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18
    ](implicit
      ccSchema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z],
      ev: ColsRepr <:< (
        A1,
        (
          A2,
          (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, Unit))))))))))))))))
        )
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]], Features.Source[Identity18]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass19[
      F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19
    ](implicit
      ccSchema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z],
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
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]], Features.Source[Identity18]], Features.Source[Identity19]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass20[
       F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20
    ](implicit
      ccSchema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z],
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
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]], Features.Source[Identity18]], Features.Source[Identity19]], Features.Source[Identity20]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21
    ](implicit
      ccSchema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z],
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
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]], Features.Source[Identity18]], Features.Source[Identity19]], Features.Source[Identity20]], Features.Source[Identity21]], 
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}

    implicit def caseClass22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z, ColsRepr, AllColumnIdentities, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21, Identity22
    ](implicit
      ccSchema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z],
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
      ),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1], Features.Source[Identity2]], Features.Source[Identity3]], Features.Source[Identity4]],  Features.Source[Identity5]], Features.Source[Identity6]], Features.Source[Identity7]], Features.Source[Identity8]], Features.Source[Identity9]], Features.Source[Identity10]], Features.Source[Identity11]], Features.Source[Identity12]], Features.Source[Identity13]], Features.Source[Identity14]], Features.Source[Identity15]], Features.Source[Identity16]], Features.Source[Identity17]], Features.Source[Identity18]], Features.Source[Identity19]], Features.Source[Identity20]], Features.Source[Identity21]], Features.Source[Identity22]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21 with Identity22
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities] {}
  }
  // format: on
}
