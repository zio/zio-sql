package zio.sql

import scala.language.implicitConversions

trait SelectModule { self: ExprModule with TableModule =>

  sealed case class SelectBuilder[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) {

    def from[A1 <: A](table: Table.Aux[A1]): Read.Select[F, A1, B] =
      Read.Select(selection, table, true, Nil)
  }

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+A <: SelectionSet[_]] extends Renderable { self =>
    type ResultType

    def union[A1 >: A <: SelectionSet[_]](that: Read[A1]): Read[A1] = Read.Union(self, that, true)

    def unionAll[A1 >: A <: SelectionSet[_]](that: Read[A1]): Read[A1] = Read.Union(self, that, false)
  }

  object Read {
    type Aux[ResultType0, +A <: SelectionSet[_]] = Read[A] {
      type ResultType = ResultType0
    }

    sealed case class Select[F, A, B <: SelectionSet[A]](
      selection: Selection[F, A, B],
      table: Table.Aux[A],
      whereExpr: Expr[_, A, Boolean],
      groupBy: List[Expr[_, A, Any]],
      havingExpr: Expr[_, A, Boolean] = true,
      orderBy: List[Ordering[Expr[_, A, Any]]] = Nil,
      offset: Option[Long] = None, //todo don't know how to do this outside of postgres/mysql
      limit: Option[Long] = None
    ) extends Read[B] { self =>
      type ResultType = selection.value.ResultTypeRepr

      /*todo need to add dialect to render/render builder - limit is represented as:
        SQL Server:
          select top x ...
        MySQL/Postgre: at the end
          select ... limit x
        Oracle: part of the where clause
          select ... where rownum <= x
       */
      override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        builder.append("select ")
        selection.renderBuilder(builder, mode)
        builder.append(" from ")
        table.renderBuilder(builder, mode)
        whereExpr match {
          case Expr.Literal(true) => ()
          case _                  =>
            builder.append(" where ")
            whereExpr.renderBuilder(builder, mode)
        }

        limit match {
          case Some(limit) =>
            builder.append(" limit ")
            offset match {
              case Some(offset) =>
                val _ = builder.append(offset).append(", ")
              case None         => ()
            }
            val _ = builder.append(limit)

          case None        => ()
        }
      }

      def where(whereExpr2: Expr[_, A, Boolean]): Select[F, A, B] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Select[F, A, B] = copy(limit = Some(n))

      def offset(n: Long): Select[F, A, B] = copy(offset = Some(n))

      def orderBy(o: Ordering[Expr[_, A, Any]], os: Ordering[Expr[_, A, Any]]*): Select[F, A, B] =
        copy(orderBy = self.orderBy ++ (o :: os.toList))

      def groupBy(key: Expr[_, A, Any], keys: Expr[_, A, Any]*)(implicit
        ev: Features.IsAggregated[F]
      ): Select[F, A, B] = {
        val _ = ev
        copy(groupBy = groupBy ++ (key :: keys.toList))
      }

      def having(havingExpr2: Expr[_, A, Boolean])(implicit
        ev: Features.IsAggregated[F]
      ): Select[F, A, B] =
        copy(havingExpr = self.havingExpr && havingExpr2)
    }

    sealed case class Union[B <: SelectionSet[_]](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B] {
      type ResultType = left.ResultType

      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {

        left.renderBuilder(builder, mode)
        builder.append(" union ")
        if (!distinct) builder.append("all ")
        right.renderBuilder(builder, mode)
      }
    }

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[SelectionSet.Singleton[Any, B]] {
      type ResultType = (B, Unit)

      def typeTag: TypeTag[B] = implicitly[TypeTag[B]]

      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" (").append(values.mkString(",")).append(") ") //todo fix
      }
    }

    def lit[B: TypeTag](values: B*): Read[SelectionSet.Singleton[Any, B]] = Literal(values.toSeq)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[F, -A, +B <: SelectionSet[A]](value: B) extends Renderable { self =>

    override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      val _ = value.renderBuilder(builder, mode)
    }

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

  sealed trait ColumnSelection[-A, +B] extends Renderable {
    def name: Option[ColumnName]
  }

  object ColumnSelection {
    sealed case class Constant[A: TypeTag](value: A, name: Option[ColumnName]) extends ColumnSelection[Any, A] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

      override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        builder.append(value.toString())
        name match {
          case Some(name) =>
            val _ = builder.append(" as ").append(name)
          case None       => ()
        }
      }

    }
    sealed case class Computed[F, A, B](expr: Expr[F, A, B], name: Option[ColumnName]) extends ColumnSelection[A, B] {
      def typeTag: TypeTag[B] = Expr.typeTagOf(expr)

      override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        expr.renderBuilder(builder, mode)
        name match {
          case Some(name) =>
            Expr.exprName(expr) match {
              case Some(sourceName) if name != sourceName =>
                val _ = builder.append(" as ").append(name)
              case _                                      => ()
            }
          case _          => ()
        }
      }

    }
  }

  sealed trait SelectionSet[-Source] extends Renderable {
    type SelectionsRepr[Source1, T]

    type ResultTypeRepr

    type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
  }

  object SelectionSet {
    type Singleton[-Source, A] = Cons[Source, A, Empty]

    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {

      override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = ()

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

      override def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        head.renderBuilder(builder, mode)
        tail match {
          case _: SelectionSet.Empty.type => ()
          case _                          =>
            builder.append(", ")
            tail.renderBuilder(builder, mode)
        }
      }

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

  sealed trait Ordering[+A]

  object Ordering {
    sealed case class Asc[A](value: A)  extends Ordering[A]
    sealed case class Desc[A](value: A) extends Ordering[A]

    implicit def exprToOrdering[F, A, B](expr: Expr[F, A, B]): Ordering[Expr[F, A, B]] =
      Asc(expr)
  }
}
