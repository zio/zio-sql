package zio.sql

import scala.language.implicitConversions

trait SelectModule { self: ExprModule with TableModule =>

  sealed case class SelectBuilder[F0, Source, B <: SelectionSet[Source]](selection: Selection[F0, Source, B]) {

    def from[Source0 <: Source](table: Table.Aux[Source0])(implicit
      ev: B <:< SelectionSet.Cons[Source0, selection.value.ColumnHead, selection.value.SelectionTail]
    ): Read.Select[
      F0,
      selection.value.ResultTypeRepr,
      Source0,
      selection.value.ColumnHead,
      selection.value.SelectionTail
    ] = {
      type B0 = SelectionSet.ConsAux[
        selection.value.ResultTypeRepr,
        Source0,
        selection.value.ColumnHead,
        selection.value.SelectionTail
      ]
      val b: B0 = selection.value.asInstanceOf[B0]

      Read.Subselect(Selection[F0, Source0, B0](b), Some(table), true, Nil)
    }
  }

  object SelectBuilder {
    implicit def noTable[F, Source >: Any, B <: SelectionSet[Source]](
      selectBuilder: SelectBuilder[F, Source, B]
    )(implicit
      ev: B <:< SelectionSet.Cons[
        Source,
        selectBuilder.selection.value.ColumnHead,
        selectBuilder.selection.value.SelectionTail
      ]
    ): Read.Select[
      F,
      selectBuilder.selection.value.ResultTypeRepr,
      Source,
      selectBuilder.selection.value.ColumnHead,
      selectBuilder.selection.value.SelectionTail
    ] = {
      type B0 = SelectionSet.ConsAux[
        selectBuilder.selection.value.ResultTypeRepr,
        Source,
        selectBuilder.selection.value.ColumnHead,
        selectBuilder.selection.value.SelectionTail
      ]
      val b: B0 = selectBuilder.selection.value.asInstanceOf[B0]

      Read.Subselect(Selection[F, Source, B0](b), None, true)
    }
  }

  final class SubselectPartiallyApplied[ParentTable] {
    def apply[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SubselectBuilder[F, A, B, ParentTable] =
      SubselectBuilder(selection)
  }

  sealed case class SubselectBuilder[F, Source, B <: SelectionSet[Source], ParentTable](
    selection: Selection[F, Source, B]
  ) {
    def from[Source0](table: Table.Aux[Source0])(implicit
      ev1: Source0 with ParentTable <:< Source,
      ev2: B <:< SelectionSet.Cons[Source, selection.value.ColumnHead, selection.value.SelectionTail]
    ): Read.Subselect[
      F,
      selection.value.ResultTypeRepr,
      Source with ParentTable,
      Source0,
      selection.value.ColumnHead,
      selection.value.SelectionTail
    ] = {
      type B0 = SelectionSet.ConsAux[
        selection.value.ResultTypeRepr,
        Source with ParentTable,
        selection.value.ColumnHead,
        selection.value.SelectionTail
      ]
      val b: B0 = selection.value.asInstanceOf[B0]

      Read.Subselect(Selection[F, Source with ParentTable, B0](b), Some(table), true)
    }
  }

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+Out] { self =>
    type ResultType

    val mapper: ResultType => Out

    type ColumnHead
    type HeadIdentity
    type ColumnTail <: ColumnSet

    type CS <: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

    val columnSet: CS

    def asTable(name: TableName): Table.DerivedTable[Out, Read[Out]]

    /**
     * Maps the [[Read]] query's output to another type by providing a function
     * that can transform from the current type to the new type.
     */
    def map[Out2](f: Out => Out2): Read.Aux[ResultType, Out2] =
      Read.Mapped(self, f)

    def to[A, Target](f: A => Target)(implicit ev: Out <:< (A, Unit)): Read[Target] =
      self.map { resultType =>
        val (a, _) = ev(resultType)

        f(a)
      }

    def to[A, B, Target](
      f: (A, B) => Target
    )(implicit ev: Out <:< (A, (B, Unit))): Read[Target] =
      self.map { resultType =>
        val (a, (b, _)) = ev(resultType)

        f(a, b)
      }

    def to[A, B, C, Target](
      f: (A, B, C) => Target
    )(implicit ev: Out <:< (A, (B, (C, Unit)))): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, _))) = ev(resultType)

        f(a, b, c)
      }

    def to[A, B, C, D, Target](
      f: (A, B, C, D) => Target
    )(implicit ev: Out <:< (A, (B, (C, (D, Unit))))): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, _)))) = ev(resultType)

        f(a, b, c, d)
      }

    def to[A, B, C, D, E, Target](
      f: (A, B, C, D, E) => Target
    )(implicit ev: Out <:< (A, (B, (C, (D, (E, Unit)))))): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, _))))) = ev(resultType)

        f(a, b, c, d, e)
      }

    def to[A, B, C, D, E, F, G, H, Target](
      f: (A, B, C, D, E, F, G, H) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, _)))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h)
      }

    def to[A, B, C, D, E, F, G, H, I, Target](
      f: (A, B, C, D, E, F, G, H, I) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, _))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, _)))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, _))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, _)))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, _))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, _)))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Target
    )(implicit
      ev: Out <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, _))))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Target
    )(implicit
      ev: Out <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit)))))))))))))))))
      )
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, _)))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Target
    )(implicit
      ev: Out <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))
      )
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, _))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s)
      }

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Target
    )(implicit
      ev: Out <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit)))))))))))))))))))
      )
    ): Read[Target] =
      self.map { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, (t, _)))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      }

    def union[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
      Read.Union[ResultType, Out1](self, that, true)

    def unionAll[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
      Read.Union[ResultType, Out1](self, that, false)
  }

  object Read {
    type Aux[ResultType0, Out] = Read[Out] {
      type ResultType = ResultType0
    }

    sealed case class Mapped[Repr, Out, Out2](read: Read.Aux[Repr, Out], f: Out => Out2) extends Read[Out2] { self =>
      override type ResultType = Repr

      override val mapper = read.mapper.andThen(f)

      override type ColumnHead = read.ColumnHead
      override type ColumnTail = read.ColumnTail
      override type CS         = read.CS

      override type HeadIdentity = read.HeadIdentity

      override val columnSet: CS = read.columnSet

      override def asTable(name: TableName): Table.DerivedTable[Out2, Mapped[Repr, Out, Out2]] =
        Table.DerivedTable[Out2, Mapped[Repr, Out, Out2]](self, name)
    }

    type Select[F, Repr, Source, Head, Tail <: SelectionSet[Source]] = Subselect[F, Repr, Source, Source, Head, Tail]

    sealed case class Subselect[F, Repr, Source, Subsource, Head, Tail <: SelectionSet[Source]](
      selection: Selection[F, Source, SelectionSet.ConsAux[Repr, Source, Head, Tail]],
      table: Option[Table.Aux[Subsource]],
      whereExpr: Expr[_, Source, Boolean],
      groupBy: List[Expr[_, Source, Any]] = Nil,
      havingExpr: Expr[_, Source, Boolean] = true,
      orderBy: List[Ordering[Expr[_, Source, Any]]] = Nil,
      offset: Option[Long] = None, //todo don't know how to do this outside of postgres/mysql
      limit: Option[Long] = None
    ) extends Read[Repr] { self =>

      def where(whereExpr2: Expr[_, Source, Boolean]): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Subselect[F, Repr, Source, Subsource, Head, Tail] = copy(limit = Some(n))

      def offset(n: Long): Subselect[F, Repr, Source, Subsource, Head, Tail] = copy(offset = Some(n))

      def orderBy(
        o: Ordering[Expr[_, Source, Any]],
        os: Ordering[Expr[_, Source, Any]]*
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] =
        copy(orderBy = self.orderBy ++ (o :: os.toList))

      def groupBy(key: Expr[_, Source, Any], keys: Expr[_, Source, Any]*)(implicit
        ev: Features.IsAggregated[F]
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] = {
        val _ = ev
        copy(groupBy = groupBy ++ (key :: keys.toList))
      }

      def having(havingExpr2: Expr[_, Source, Boolean])(implicit
        ev: Features.IsAggregated[F]
      ): Subselect[F, Repr, Source, Subsource, Head, Tail] = {
        val _ = ev
        copy(havingExpr = self.havingExpr && havingExpr2)
      }

      override def asTable(
        name: TableName
      ): Table.DerivedTable[Repr, Subselect[F, Repr, Source, Subsource, Head, Tail]] =
        Table.DerivedTable[Repr, Subselect[F, Repr, Source, Subsource, Head, Tail]](self, name)

      override type ResultType = Repr

      override val mapper: Repr => Repr = identity(_)

      override type ColumnHead = selection.value.ColumnHead
      override type ColumnTail = selection.value.ColumnTail

      override type HeadIdentity = selection.value.HeadIdentity

      override val columnSet: CS = selection.value.columnSet

      override type CS = selection.value.CS
    }

    object Subselect {
      implicit def subselectToExpr[F <: Features.Aggregated[_], Repr, Source, Subsource, Head](
        subselect: Read.Subselect[F, Repr, _ <: Source, Subsource, Head, SelectionSet.Empty]
      ): Expr[F, Any, Head] =
        Expr.Subselect(subselect)
    }

    sealed case class Union[Repr, Out](left: Read.Aux[Repr, Out], right: Read.Aux[Repr, Out], distinct: Boolean)
        extends Read[Out] { self =>
      override type ResultType = Repr

      override val mapper: ResultType => Out = left.mapper

      override type ColumnHead = left.ColumnHead
      override type ColumnTail = left.ColumnTail

      override type HeadIdentity = left.HeadIdentity

      override type CS = left.CS

      override val columnSet: CS = left.columnSet

      override def asTable(name: TableName): Table.DerivedTable[Out, Union[Repr, Out]] =
        Table.DerivedTable[Out, Union[Repr, Out]](self, name)
    }

    // TODO add name to literal selection - e.g. select '1' as one
    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[(B, Unit)] { self =>
      override type ResultType = (B, Unit)

      override val mapper: ResultType => (B, Unit) = identity(_)

      def typeTag: TypeTag[B] = implicitly[TypeTag[B]]

      override type ColumnHead = B
      override type ColumnTail = ColumnSet.Empty

      override type CS = ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

      override val columnSet: CS = ColumnSet.Cons(Column.Indexed[ColumnHead, HeadIdentity](), ColumnSet.Empty)

      override def asTable(name: TableName): Table.DerivedTable[(B, Unit), Literal[B]] =
        Table.DerivedTable[(B, Unit), Literal[B]](self, name)
    }

    def lit[B: TypeTag](values: B*): Read[(B, Unit)] = Literal(values.toSeq)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[F, -A, +B <: SelectionSet[A]](value: B) { self =>

    type ColsRepr = value.ResultTypeRepr

    def ++[F2, A1 <: A, C <: SelectionSet[A1]](
      that: Selection[F2, A1, C]
    ): Selection[F :||: F2, A1, self.value.Append[A1, C]] =
      Selection(self.value ++ that.value)

  }

  object Selection {
    import ColumnSelection._
    import SelectionSet.{ Cons, Empty }

    type Aux[F, -A, +B <: SelectionSet[A], ColsRepr0] = Selection[F, A, B] {
      type ColsRepr = ColsRepr0
    }

    def constantOption[A: TypeTag](value: A, option: Option[ColumnName]): Selection[Any, Any, Cons[Any, A, Empty]] =
      Selection(Cons(Constant(value, option), Empty))

    def constant[A: TypeTag](value: A): Selection[Any, Any, Cons[Any, A, Empty]] = constantOption(value, None)

    def constantAs[A: TypeTag](value: A, name: ColumnName): Selection[Any, Any, Cons[Any, A, Empty]] =
      constantOption(value, Some(name))

    def computedOption[F, A, B](expr: Expr[F, A, B], name: Option[ColumnName]): Selection[F, A, Cons[A, B, Empty]] =
      Selection(Cons(Computed(expr, name), Empty))

    def computed[F, A, B](expr: Expr[F, A, B]): Selection[F, A, Cons[A, B, Empty]] =
      computedOption(expr, None)

    def computedAs[F, A, B](expr: Expr[F, A, B], name: ColumnName): Selection[F, A, Cons[A, B, Empty]] =
      computedOption(expr, Some(name))
  }

  sealed trait ColumnSelection[-Source, +ColumnType] {
    type ColumnType0 <: ColumnType

    def name: Option[ColumnName]

    val toColumn: Column[ColumnType]
  }

  object ColumnSelection {
    sealed case class Constant[ColumnType: TypeTag](value: ColumnType, name: Option[ColumnName])
        extends ColumnSelection[Any, ColumnType] {
      def typeTag: TypeTag[ColumnType] = implicitly[TypeTag[ColumnType]]

      val toColumn: Column[ColumnType] = name match {
        case Some(value) => Column.Named(value)
        case None        => Column.Indexed()
      }
    }

    sealed case class Computed[F, Source, ColumnType](expr: Expr[F, Source, ColumnType], name: Option[ColumnName])
        extends ColumnSelection[Source, ColumnType] {
      implicit def typeTag: TypeTag[ColumnType] = Expr.typeTagOf(expr)

      val toColumn: Column[ColumnType] = name match {
        case Some(value) => Column.Named(value)
        case None        => Column.Indexed()
      }
    }
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[Source1, T]

    type ResultTypeRepr

    type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    type ColumnHead
    type ColumnTail <: ColumnSet
    type SelectionTail <: SelectionSet[Source]
    type HeadIdentity

    type CS <: ColumnSet
    def columnSet: CS

    def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
  }

  object SelectionSet {

    type Aux[-Source, ResultTypeRepr0] =
      SelectionSet[Source] {
        type ResultTypeRepr = ResultTypeRepr0
      }

    type ConsAux[ResultTypeRepr0, -Source, A, B <: SelectionSet[Source]] =
      SelectionSet.Cons[Source, A, B] {
        type ResultTypeRepr = ResultTypeRepr0
      }

    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {

      override type ColumnHead    = Unit
      override type ColumnTail    = ColumnSet.Empty
      override type SelectionTail = SelectionSet.Empty

      override type HeadIdentity = Any

      override type CS = ColumnSet.Empty
      override def columnSet: CS = ColumnSet.Empty

      override type SelectionsRepr[Source1, T] = Unit

      override type ResultTypeRepr = Unit

      override type Append[Source1, That <: SelectionSet[Source1]] = That

      override def ++[Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      override def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
    }

    sealed case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
        extends SelectionSet[Source] { self =>

      override type ColumnHead    = A
      override type ColumnTail    = tail.CS
      override type SelectionTail = B

      override type HeadIdentity = head.toColumn.Identity

      override type CS = ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity]

      override def columnSet: CS =
        ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity](head.toColumn, tail.columnSet)

      override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

      override type ResultTypeRepr = (A, tail.ResultTypeRepr)

      override type Append[Source1, That <: SelectionSet[Source1]] =
        Cons[Source1, A, tail.Append[Source1, That]]

      override def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      override def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
    }
  }

  sealed trait Ordering[+A] {
    val value: A
  }

  object Ordering {
    sealed case class Asc[A](value: A)  extends Ordering[A]
    sealed case class Desc[A](value: A) extends Ordering[A]

    implicit def exprToOrdering[F, A, B](expr: Expr[F, A, B]): Ordering[Expr[F, A, B]] =
      Asc(expr)
  }

  sealed trait DecodingError extends Exception {
    def message: String
  }

  object DecodingError {
    sealed case class UnexpectedNull(column: Either[Int, String])       extends DecodingError {
      private def label = column.fold(index => index.toString, name => name)

      def message = s"Expected column ${label} to be non-null"
    }
    sealed case class UnexpectedType(expected: TypeTag[_], actual: Int) extends DecodingError {
      def message = s"Expected type ${expected} but found ${actual}"
    }
    sealed case class MissingColumn(column: Either[Int, String])        extends DecodingError {
      private def label = column.fold(index => index.toString, name => name)

      def message = s"The column ${label} does not exist"
    }
    case object Closed                                                  extends DecodingError {
      def message = s"The ResultSet has been closed, so decoding is impossible"
    }
  }
}
