package zio.sql

import scala.language.implicitConversions

/**
 *
 * TODO
 *  1. add to column type capability to contain null values
 *  2. add auto generated capabilities (like identity for postgresql)
 *  3. foreign key...
 *  4. insert multiple rows at once
 *  5. translate for other modules than just postgres
 *  6. OPTIONAL we could compare table's "AllColumnIdentities" with Expr[Features.Source[Identity]] instead of comparing length and uniqness (decide what makes most sense with regards to other points)
 *
 * TODO research => there exists also complex inserts - values could be "subselect" that returns values ....
 *      Insert Into Test (Test_Date, Testno, Examno, Serialno, Type, Hours)
 *      Select S.Test_Date, E.Testno, S.Examno, S.Serialno, 'Non-Flight', (F.STARTED- F.ENDED) as Hours
 *      From Semester S, TIME F, TESTPAPERS e
 *      Where S.Testno = F.Testno And E.Testno = 1
 * 
 *  insert into customers
 *      (id, first_name, last_name, verified, dob, created_timestamp_string, created_timestamp)
 *  values
 *      ('60b01fc9-c902-4468-8d49-3c0f989def37', 'Ronald', 'Russell', true, '1983-01-05', '2020-11-21T19:10:25+00:00', '2020-11-21 19:10:25+00'),
 *      ('f76c9ace-be07-4bf3-bd4c-4a9c62882e64', 'Terrence', 'Noel', true, '1999-11-02', '2020-11-21T15:10:25-04:00', '2020-11-21 15:10:25-04'))
 */
trait InsertAltModule { self: ExprModule with TableModule =>

  sealed case class InsertAltBuilder[Source, N <: ColumnCount, AllColumnIdentities](table: Table.Source.AuxN[Source, AllColumnIdentities, N]) {
    def values(values: InsertRow[Source])(implicit ev: values.Size =:= N): InsertAlt[Source] = InsertAlt(table, values)
  }

  sealed trait InsertRow[-Source] { self =>

    type Append[Source1, Appended, ThatIdentity] <: InsertRow[Source1]

    type Size <: ColumnCount

    def ++[Source1 <: Source, Appended, ThatIdentity](
      that: (Expr[Features.Source[ThatIdentity], Source1, Appended], Appended)
    )(implicit unique: Unique[ThatIdentity, self.type], ev: TypeTag[Appended]): Append[Source1, Appended, ThatIdentity]
  }

  object InsertRow {
    type Empty = Empty.type
    import ColumnCount._

    case object Empty extends InsertRow[Any] {
      override type Size                                    = _0
      override type Append[Source1, Appended, ThatIdentity] =
        InsertRow.Cons[Source1, Appended, InsertRow.Empty, ThatIdentity]

      override def ++[Source1 <: Any, Appended, ThatIdentity](
        that: (Expr[Features.Source[ThatIdentity], Source1, Appended], Appended)
      )(implicit unique: Unique[ThatIdentity, Empty], ev: TypeTag[Appended]): Append[Source1, Appended, ThatIdentity] =
        InsertRow.Cons(that, InsertRow.Empty)
    }

    sealed case class Cons[-Source, H: TypeTag, T <: InsertRow[Source], HeadIdentity](
      tupleHead: (Expr[Features.Source[HeadIdentity], Source, H], H),
      tail: T
    ) extends InsertRow[Source] { self =>

      override type Size = Succ[tail.Size]

      override type Append[Source1, Appended, ThatIdentity] =
        InsertRow.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity]

      override def ++[Source1 <: Source, Appended, ThatIdentity](
        that: (Expr[Features.Source[ThatIdentity], Source1, Appended], Appended)
      )(implicit
        unique: Unique[ThatIdentity, self.type], ev: TypeTag[Appended]
        ): InsertRow.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity] =
        InsertRow.Cons[Source1, H, tail.Append[Source1, Appended, ThatIdentity], HeadIdentity](
          tupleHead,
          tail.++(that)(new Unique[ThatIdentity, T] {}, implicitly[TypeTag[Appended]])
        )
    }
  }

  implicit def tupleToInsertSet[Source, H: TypeTag, HeadIdentity](
    tupleHead: (Expr[Features.Source[HeadIdentity], Source, H], H)
  ): InsertRow.Cons[Source, H, InsertRow.Empty, HeadIdentity] =
    InsertRow.Cons(tupleHead, InsertRow.Empty)

  sealed case class InsertAlt[A](table: Table.Source.Aux[A], values: InsertRow[A])

  sealed trait Unique[A, -Origin <: InsertRow[_]]

  object Unique {
    implicit def uniqueInEmpty[A]: Unique[A, InsertRow.Empty] =
      new Unique[A, InsertRow.Empty] {}

    implicit def uniqueInCons[A, H, T <: InsertRow[_]](implicit
      tailNotContain: Unique[H, T],
      ev: A =!= H
    ): Unique[A, InsertRow.Cons[_, _, T, H]]                     =
      new Unique[A, InsertRow.Cons[_, _, T, H]] {}
  }

  sealed trait =!=[A, B]

  object =!= {
    implicit def neq[A, B]: A =!= B = null

    // This pair excludes the A =:= B case
    implicit def neqAmbig1[A]: A =!= A = null
    implicit def neqAmbig2[A]: A =!= A = null
  }
}
