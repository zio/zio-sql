package zio.sql.select

import zio.sql.macros._
import zio.sql.Features
import zio.sql.table._
import scala.language.implicitConversions

import zio.sql.typetag.TypeTag
import zio.sql.expr.Expr

/**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
sealed trait Read[+Out] { self =>
  type ResultType

  val mapper: ResultType => Out

  type ColumnsOut

  type TableSource

  def columns(name: String): ColumnsOut

  /**
     * Maps the [[Read]] query's output to another type by providing a function
     * that can transform from the current type to the new type.
     */
  def map[Out2](f: Out => Out2): Read.Aux[ResultType, Out2] =
    Read.Mapped(self, f)

  def asTable(
    name: String
  ): Table.DerivedTable[ColumnsOut, Out, Read[Out] { type ColumnsOut = self.ColumnsOut }, TableSource]

  def to[Target](f: Out => Target): Read[Target] =
    self.map { resultType =>
      f(resultType)
    }

  def union[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
    Read.Union[ResultType, Out1](self, that, distinct = true)

  def unionAll[Out1 >: Out](that: Read.Aux[ResultType, Out1]): Read.Aux[ResultType, Out1] =
    Read.Union[ResultType, Out1](self, that, distinct = false)
}

object Read {
  sealed trait ExprSet[-Source] {
    type Append[F2, Source1 <: Source, B2] <: ExprSet[Source1]
    def ++[F2, Source1 <: Source, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2]
  }

  object ExprSet {
    type NoExpr = NoExpr.type
    case object NoExpr extends ExprSet[Any] {
      override type Append[F2, Source1 <: Any, B2] = ExprCons[F2, Source1, B2, NoExpr]

      override def ++[F2, Source1 <: Any, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2] =
        ExprCons(that, NoExpr)
    }

    final case class ExprCons[F, Source, B, T <: ExprSet[Source]](head: Expr[F, Source, B], tail: T)
        extends ExprSet[Source] {
      override type Append[F2, Source1 <: Source, B2] =
        ExprCons[F, Source1, B, tail.Append[F2, Source1, B2]]
      override def ++[F2, Source1 <: Source, B2](that: Expr[F2, Source1, B2]): Append[F2, Source1, B2] =
        ExprCons(head, tail.++[F2, Source1, B2](that))
    }
  }

  type WithReprs[+Out, Reprs] = Read[Out] {
    type ColumnsOut = Reprs
  }

  type Aux[ResultType0, Out] = Read[Out] {
    type ResultType = ResultType0
  }

  final case class Mapped[Repr, Out, Out2](read: Read.Aux[Repr, Out], f: Out => Out2) extends Read[Out2] { self =>
    override type ResultType = Repr

    override val mapper = read.mapper.andThen(f)

    override type TableSource = read.TableSource

    override type ColumnsOut = read.ColumnsOut
    override def columns(name: String): ColumnsOut = read.columns(name)

    override def asTable(name: String): Table.DerivedTable[read.ColumnsOut, Out2, Mapped[
      Repr,
      Out,
      Out2
    ] { type ColumnsOut = self.ColumnsOut }, read.TableSource] =
      Table.DerivedTable[self.ColumnsOut, Out2, Mapped[
        Repr,
        Out,
        Out2
      ] { type ColumnsOut = self.ColumnsOut }, read.TableSource](
        self,
        name
      )
  }

  type Select[F, Repr, Source, Head, Tail <: SelectionSet[Source]] = Subselect[F, Repr, Source, Source, Head, Tail]

  sealed case class Subselect[F, Repr, Source, Subsource, Head, Tail <: SelectionSet[Source]](
    selection: Selection[F, Source, SelectionSet.ConsAux[Repr, Source, Head, Tail]],
    table: Option[Table.Aux[Subsource]],
    whereExpr: Expr[_, Source, Boolean],
    groupByExprs: ExprSet[Source] = ExprSet.NoExpr,
    havingExpr: Expr[_, Source, Boolean] = true,
    orderByExprs: List[Ordering[Expr[_, Source, Any]]] = Nil,
    offset: Option[Long] = None, // todo don't know how to do this outside of postgres/mysql
    limit: Option[Long] = None
  ) extends Read[Repr] { self =>

    type GroupByF <: Any

    def where[F2](
      whereExpr2: Expr[F2, Source, Boolean]
    )(implicit
      ev: WhereIsSound[F2, self.GroupByF]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF] =
      new Subselect(
        selection,
        table,
        self.whereExpr && whereExpr2,
        groupByExprs,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF
      }

    def limit(n: Long): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF] =
      new Subselect(selection, table, whereExpr, groupByExprs, havingExpr, orderByExprs, offset, Some(n)) {
        override type GroupByF = self.GroupByF
      }

    def offset(n: Long): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF] =
      new Subselect(selection, table, whereExpr, groupByExprs, havingExpr, orderByExprs, Some(n), limit) {
        override type GroupByF = self.GroupByF
      }

    def orderBy(
      o: Ordering[Expr[_, Source, Any]],
      os: Ordering[Expr[_, Source, Any]]*
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF] =
      new Subselect(
        selection,
        table,
        whereExpr,
        groupByExprs,
        havingExpr,
        self.orderByExprs ++ (o :: os.toList),
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF
      }

    def having[F2](
      havingExpr2: Expr[F2, Source, Boolean]
    )(implicit
      ev: HavingIsSound[F, self.GroupByF, F2]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF] =
      new Subselect(
        selection,
        table,
        whereExpr,
        groupByExprs,
        self.havingExpr && havingExpr2,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF
      }

    def groupBy[F1](expr1: Expr[F1, Source, Any])(implicit
      verify: GroupByLike[F, F1]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF with F1] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1
      }

    def groupBy[F1, F2](expr1: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any])(implicit
      verify: GroupByLike[F, F1 with F2]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF with F1 with F2] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2
      }

    def groupBy[F1, F2, F3](expr1: Expr[F1, Source, Any], expr2: Expr[F2, Source, Any], expr3: Expr[F3, Source, Any])(
      implicit verify: GroupByLike[F, F1 with F2 with F3]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF with F1 with F2 with F3] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2 ++ expr3,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2 with F3
      }

    def groupBy[F1, F2, F3, F4](
      expr1: Expr[F1, Source, Any],
      expr2: Expr[F2, Source, Any],
      expr3: Expr[F3, Source, Any],
      expr4: Expr[F4, Source, Any]
    )(implicit
      verify: GroupByLike[F, F1 with F2 with F3 with F4]
    ): Subselect.WithGroupByF[F, Repr, Source, Subsource, Head, Tail, self.GroupByF with F1 with F2 with F3 with F4] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2 ++ expr3 ++ expr4,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2 with F3 with F4
      }

    def groupBy[F1, F2, F3, F4, F5](
      expr1: Expr[F1, Source, Any],
      expr2: Expr[F2, Source, Any],
      expr3: Expr[F3, Source, Any],
      expr4: Expr[F4, Source, Any],
      expr5: Expr[F5, Source, Any]
    )(implicit verify: GroupByLike[F, F1 with F2 with F3 with F4 with F5]): Subselect.WithGroupByF[
      F,
      Repr,
      Source,
      Subsource,
      Head,
      Tail,
      self.GroupByF with F1 with F2 with F3 with F4 with F5
    ] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2 with F3 with F4 with F5
      }

    def groupBy[F1, F2, F3, F4, F5, F6](
      expr1: Expr[F1, Source, Any],
      expr2: Expr[F2, Source, Any],
      expr3: Expr[F3, Source, Any],
      expr4: Expr[F4, Source, Any],
      expr5: Expr[F5, Source, Any],
      expr6: Expr[F6, Source, Any]
    )(implicit verify: GroupByLike[F, F1 with F2 with F3 with F4 with F5 with F6]): Subselect.WithGroupByF[
      F,
      Repr,
      Source,
      Subsource,
      Head,
      Tail,
      self.GroupByF with F1 with F2 with F3 with F4 with F5 with F6
    ] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2 with F3 with F4 with F5 with F6
      }

    // TODO add arities up to 22 if needed
    def groupBy[F1, F2, F3, F4, F5, F6, F7](
      expr1: Expr[F1, Source, Any],
      expr2: Expr[F2, Source, Any],
      expr3: Expr[F3, Source, Any],
      expr4: Expr[F4, Source, Any],
      expr5: Expr[F5, Source, Any],
      expr6: Expr[F6, Source, Any],
      expr7: Expr[F7, Source, Any]
    )(implicit verify: GroupByLike[F, F1 with F2 with F3 with F4 with F5 with F6 with F7]): Subselect.WithGroupByF[
      F,
      Repr,
      Source,
      Subsource,
      Head,
      Tail,
      self.GroupByF with F1 with F2 with F3 with F4 with F5 with F6 with F7
    ] =
      new Subselect[F, Repr, Source, Subsource, Head, Tail](
        selection,
        table,
        whereExpr,
        self.groupByExprs ++ expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7,
        havingExpr,
        orderByExprs,
        offset,
        limit
      ) {
        override type GroupByF = self.GroupByF with F1 with F2 with F3 with F4 with F5 with F6 with F7
      }

    def normalize(implicit
      instance: Normalizer[ResultType]
    ): Subselect[F, instance.Out, Source, Subsource, Head, Tail] =
      self.asInstanceOf[Subselect[F, instance.Out, Source, Subsource, Head, Tail]]

    override def asTable(
      name: String
    ): Table.DerivedTable[selection.ColumnsOut[Source], Repr, Subselect[
      F,
      Repr,
      Source,
      Subsource,
      Head,
      Tail
    ] { type ColumnsOut = self.ColumnsOut }, Source] =
      Table.DerivedTable[self.ColumnsOut, Repr, Subselect[
        F,
        Repr,
        Source,
        Subsource,
        Head,
        Tail
      ] { type ColumnsOut = self.ColumnsOut }, Source](self, name)

    override type ResultType = Repr

    override type TableSource = Source

    override val mapper: Repr => Repr = identity(_)

    override type ColumnsOut = selection.ColumnsOut[Source]
    override def columns(name: String) = selection.columns[Source](name)
  }

  object Subselect {

    type WithGroupByF[F, Repr, Source, Subsource, Head, Tail <: SelectionSet[Source], GroupByF0] =
      Subselect[F, Repr, Source, Subsource, Head, Tail] {
        type GroupByF = GroupByF0
      }

    implicit def subselectToExpr[F <: Features.Aggregated[_], Repr, Source, Subsource, Head](
      subselect: Read.Subselect[F, Repr, _ <: Source, Subsource, Head, SelectionSet.Empty]
    ): Expr[Features.Derived, Any, Head] =
      Expr.Subselect(subselect)
  }

  final case class Union[Repr, Out](left: Read.Aux[Repr, Out], right: Read.Aux[Repr, Out], distinct: Boolean)
      extends Read[Out] { self =>
    override type ResultType = Repr

    override val mapper: ResultType => Out = left.mapper

    override type ColumnsOut = left.ColumnsOut

    override type TableSource = left.TableSource

    override def columns(name: String): ColumnsOut = left.columns(name)

    override def asTable(name: String): Table.DerivedTable[left.ColumnsOut, Out, Union[
      Repr,
      Out
    ] { type ColumnsOut = self.ColumnsOut }, left.TableSource] =
      Table
        .DerivedTable[self.ColumnsOut, Out, Union[Repr, Out] { type ColumnsOut = self.ColumnsOut }, left.TableSource](
          self,
          name
        )
  }

  final case class Literal[B: TypeTag](values: Iterable[B]) extends Read[B] { self =>
    override type ResultType = B

    override val mapper: ResultType => B = identity(_)

    type ColumnIdentity

    def typeTag: TypeTag[B] = implicitly[TypeTag[B]]

    override type ColumnsOut = Expr[Features.Source[ColumnIdentity, Any], Any, B]
    override def columns(name: String) = Expr.Source(name, Column.Indexed[B, ColumnIdentity]())

    override type TableSource = Any

    override def asTable(
      name: String
    ): Table.DerivedTable[Expr[Features.Source[ColumnIdentity, Any], Any, B], B, Literal[
      B
    ] { type ColumnsOut = self.ColumnsOut }, Any] =
      Table.DerivedTable[Expr[Features.Source[ColumnIdentity, Any], Any, B], B, Literal[
        B
      ] { type ColumnsOut = self.ColumnsOut }, Any](self, name)
  }

  implicit def seqToLiteral[B](values: Seq[B])(implicit typeTag: TypeTag[B]): Read[B] =
    Read.Literal[B](values)
}
