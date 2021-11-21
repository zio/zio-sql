package zio.sql

import scala.language.implicitConversions

/**
 * insert into customers (id, first_name, last_name, verified, dob)
 *   values ('22e37786-af3b-451e-80b4-0baf41c0933e', 'Jaro', 'Regec', 0, '1983-01-05')
 *
 * insertInto(customers)(customerId ++ fName ++ lName ++ verified ++ dob)
 *   .values('22e37786-af3b-451e-80b4-0baf41c0933e', 'Jaro', 'Regec', 0, '1983-01-05')
 *
 * OR
 *
 * insertInto(customers){fName ++ lName ++ dob} values ("Joe" ++ "Bloggs" ++ LocalDate.of(1990, 1, 8), "Matt" ++ "Smith" ++ LocalDate.of(1978, 4, 5))
 *
 * OR
 *  we need all the values for non null & not auto generated columns
 *  we also need columns to link values with columns
 *  could value remember its source? -> maybe then we wouln't need columns at all like:
 *
 *  insertInto(customers).values('22e37786-af3b-451e-80b4-0baf41c0933e', 'Jaro', 'Regec', 0, '1983-01-05')
 *
 * Columns - Values could be:
 *   - selections set -> but would consist only of Expr of type Source
 *   - column set     -> we don't currently have a way to concat raw columns
 *   - own solution   -> like InsertRow below
 *
 *    insertInto(customers)
 *       .values(
 *        (customerId         -> java.util.UUID.fromString("28e880be-c783-43ea-9839-db51834347a8")) ++
 *        (dob                -> LocalDate.now()) ++
 *        (fName              -> "Jaro") ++
 *        (lName              -> "Regec") ++
 *        (verified           -> true) ++
 *        (createdString      -> s"${ZonedDateTime.now().toInstant().toEpochMilli()}") ++
 *        (createdTimestamp   -> ZonedDateTime.now()))
 *
 * TODO
 *  1. add to column type capability to contain null values
 *  2. add auto generated capabilities (like identity for postgresql)
 *  3. foreign key...
 *
 * TODO research => there exists also complex inserts - values could be "subselect" that returns values ....
 *      Insert Into Test (Test_Date, Testno, Examno, Serialno, Type, Hours)
 *      Select S.Test_Date, E.Testno, S.Examno, S.Serialno, 'Non-Flight', (F.STARTED- F.ENDED) as Hours
 *      From Semester S, TIME F, TESTPAPERS e
 *      Where S.Testno = F.Testno And E.Testno = 1
 */
trait InsertModule { self: ExprModule with TableModule =>

  sealed case class InsertBuilder[Source, N <: ColumnCount](table: Table.Source.Aux[Source, N]) {
    def values(values: InsertRow[Source])(implicit ev: values.Size =:= N): Insert[Source, N] = Insert(table, values)
  }

  sealed trait =!=[A, B]

  object =!= {
    implicit def neq[A, B]: A =!= B = null

    // This pair excludes the A =:= B case
    implicit def neqAmbig1[A]: A =!= A = null
    implicit def neqAmbig2[A]: A =!= A = null
  }

  sealed trait InsertRow[-Source] { self =>

    type Append[Source1, Appended] <: InsertRow[Source1]

    type Size <: ColumnCount

    def ++[Source1 <: Source, Appended](
      that: (Expr[Features.Source, Source1, Appended], Appended)
    )(implicit unique: Unique[Appended, self.type]): Append[Source1, Appended]
  }
  object InsertRow {
    type Empty = Empty.type
    import ColumnCount._

    case object Empty extends InsertRow[Any] {
      override type Size                      = _0
      override type Append[Source1, Appended] = InsertRow.Cons[Source1, Appended, InsertRow.Empty]

      override def ++[Source1 <: Any, Appended](
        that: (Expr[Features.Source, Source1, Appended], Appended)
      )(implicit unique: Unique[Appended, Empty]): Append[Source1, Appended] =
        InsertRow.Cons(that, InsertRow.Empty)
    }

    sealed case class Cons[-Source, H, T <: InsertRow[Source]](
      tupleHead: (Expr[Features.Source, Source, H], H),
      tail: T
    ) extends InsertRow[Source] { self =>

      override type Size = Succ[tail.Size]

      override type Append[Source1, Appended] = InsertRow.Cons[Source1, H, tail.Append[Source1, Appended]]

      override def ++[Source1 <: Source, Appended](
        that: (Expr[Features.Source, Source1, Appended], Appended)
      )(implicit unique: Unique[Appended, self.type]): InsertRow.Cons[Source1, H, tail.Append[Source1, Appended]] =
        InsertRow.Cons[Source1, H, tail.Append[Source1, Appended]](tupleHead, tail.++(that)(new Unique[Appended, T] {}))
    }
  }

  //TODO translate
  sealed case class Insert[A, N <: ColumnCount](table: Table.Source.Aux[A, N], values: InsertRow[A])

  implicit def tupleToInsertSet[Source, H](
    tupleHead: (Expr[Features.Source, Source, H], H)
  ): InsertRow.Cons[Source, H, InsertRow.Empty] =
    InsertRow.Cons(tupleHead, InsertRow.Empty)

  //TODO works but following are the same types and therefore rejected
  // (fName -> "Jaro")  => (Expr[Features.Source, TableType, String], String)
  // (lName -> "Regec") => (Expr[Features.Source, TableType, String], String)
  sealed trait Unique[A, -Origin <: InsertRow[_]]

  object Unique {
    implicit def uniqueInEmpty[A]: Unique[A, InsertRow.Empty] =
      new Unique[A, InsertRow.Empty] {}

    implicit def uniqueInCons[A, H, T <: InsertRow[_]](implicit
      tailNotContain: Unique[A, T],
      ev: A =!= H
    ): Unique[A, InsertRow.Cons[_, H, T]]                     =
      new Unique[A, InsertRow.Cons[_, H, T]] {}
  }
}
