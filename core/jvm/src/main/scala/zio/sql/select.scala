package zio.sql

import scala.language.implicitConversions

trait SelectModule { self: ExprModule with TableModule =>
  sealed case class SelectBuilder[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) {

    // [F, Repr, A, B <: SelectionSet.Aux[Repr, A]]
    def from[A1 <: A](table: Table.Aux[A1]): Read.Select[F, selection.value.ResultTypeRepr, A1] = {
      type B0 = SelectionSet.Aux[selection.value.ResultTypeRepr, A]
      val b: B0 = selection.value

      Read.Select(Selection[F, A1, B0](b), table, true, Nil)
    }
  }

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+Out] { self =>
    type ResultType

    val mapper: ResultType => Out

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

    sealed case class Mapped[Repr, Out, Out2](read: Read.Aux[Repr, Out], f: Out => Out2) extends Read[Out2] {
      type ResultType = Repr

      val mapper = read.mapper.andThen(f)
    }

    sealed case class Select[F, Repr, A](
      selection: Selection[F, A, SelectionSet.Aux[Repr, A]],
      table: Table.Aux[A],
      whereExpr: Expr[_, A, Boolean],
      groupBy: List[Expr[_, A, Any]],
      havingExpr: Expr[_, A, Boolean] = true,
      orderBy: List[Ordering[Expr[_, A, Any]]] = Nil,
      offset: Option[Long] = None, //todo don't know how to do this outside of postgres/mysql
      limit: Option[Long] = None
    ) extends Read[Repr] { self =>
      type ResultType = Repr

      val mapper: Repr => Repr = identity(_)

      def where(whereExpr2: Expr[_, A, Boolean]): Select[F, Repr, A] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Select[F, Repr, A] = copy(limit = Some(n))

      def offset(n: Long): Select[F, Repr, A] = copy(offset = Some(n))

      def orderBy(o: Ordering[Expr[_, A, Any]], os: Ordering[Expr[_, A, Any]]*): Select[F, Repr, A] =
        copy(orderBy = self.orderBy ++ (o :: os.toList))

      def groupBy(key: Expr[_, A, Any], keys: Expr[_, A, Any]*)(implicit
        ev: Features.IsAggregated[F]
      ): Select[F, Repr, A] = {
        val _ = ev
        copy(groupBy = groupBy ++ (key :: keys.toList))
      }

      def having(havingExpr2: Expr[_, A, Boolean])(implicit
        ev: Features.IsAggregated[F]
      ): Select[F, Repr, A] = {
        val _ = ev
        copy(havingExpr = self.havingExpr && havingExpr2)
      }
    }

    sealed case class Union[Repr, Out](left: Read.Aux[Repr, Out], right: Read.Aux[Repr, Out], distinct: Boolean)
        extends Read[Out] {
      type ResultType = Repr

      val mapper: ResultType => Out = left.mapper
    }

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[(B, Unit)] {
      type ResultType = (B, Unit)

      val mapper: ResultType => (B, Unit) = identity(_)

      def typeTag: TypeTag[B] = implicitly[TypeTag[B]]
    }

    def lit[B: TypeTag](values: B*): Read[(B, Unit)] = Literal(values.toSeq)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[F, -A, +B <: SelectionSet[A]](value: B) { self =>

    type SelectionType

    def ++[F2, A1 <: A, C <: SelectionSet[A1]](
      that: Selection[F2, A1, C]
    ): Selection[F :||: F2, A1, self.value.Append[A1, C]] =
      Selection(self.value ++ that.value)

    def columns[A1 <: A]: value.SelectionsRepr[A1, SelectionType] = value.selections[A1, SelectionType]
  }

  object Selection {
    import SelectionSet.{ Cons, Empty }
    import ColumnSelection._

    val empty: Selection[Any, Any, Empty] = Selection(Empty)

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

  sealed trait ColumnSelection[-A, +B] {
    def name: Option[ColumnName]
  }

  object ColumnSelection {

    sealed case class Constant[A: TypeTag](value: A, name: Option[ColumnName]) extends ColumnSelection[Any, A] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }

    sealed case class Computed[F, A, B](expr: Expr[F, A, B], name: Option[ColumnName]) extends ColumnSelection[A, B] {
      def typeTag: TypeTag[B] = Expr.typeTagOf(expr)
    }
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[Source1, T]

    type ResultTypeRepr

    type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
  }

  object SelectionSet {
    type Aux[ResultTypeRepr0, -Source] =
      SelectionSet[Source] {
        type ResultTypeRepr = ResultTypeRepr0
      }

    type Singleton[-Source, A] = Cons[Source, A, Empty]

    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {

      override type SelectionsRepr[Source1, T] = Unit

      override type ResultTypeRepr = Unit

      override type Append[Source1, That <: SelectionSet[Source1]] = That

      override def ++[Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
    }

    sealed case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
        extends SelectionSet[Source] { self =>

      override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

      override type ResultTypeRepr = (A, tail.ResultTypeRepr)

      override type Append[Source1, That <: SelectionSet[Source1]] =
        Cons[Source1, A, tail.Append[Source1, That]]

      override def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
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
