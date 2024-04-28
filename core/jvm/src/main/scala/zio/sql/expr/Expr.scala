package zio.sql.expr

import scala.language.implicitConversions
import zio.sql.table._
import zio.sql.typetag._
import zio.sql.ops.Operator._
import zio.sql.select._
import zio.sql.Features
import zio.sql.select.{ Read, Selection, SelectionSet }

/**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
sealed trait Expr[-F, -A, +B] { self =>

  def +[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.Add[B1]())

  def -[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.Sub[B1]())

  def *[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.Mul[B1]())

  // todo do something special for divide by 0? also Mod/log/whatever else is really a partial function.. PartialExpr?
  def /[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.Div[B1]())

  def &&[F2, A1 <: A, B1 >: B](
    that: Expr[F2, A1, Boolean]
  )(implicit ev: B <:< Boolean): Expr[F with F2, A1, Boolean] =
    Expr.Binary(self.widen[Boolean], that, BinaryOp.AndBool)

  def ||[F2, A1 <: A, B1 >: B](
    that: Expr[F2, A1, Boolean]
  )(implicit ev: B <:< Boolean): Expr[F with F2, A1, Boolean] =
    Expr.Binary(self.widen[Boolean], that, BinaryOp.OrBool)

  def ===[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.Equals)

  def <>[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.NotEqual)

  def >[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.GreaterThan)

  def <[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.LessThan)

  def >=[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.GreaterThanEqual)

  def <=[F2, A1 <: A, B1 >: B, B2](that: Expr[F2, A1, B2])(implicit
    ct: ComparableTypes[B1, B2]
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.LessThanEqual)

  def soundsLike[F2, A1 <: A](that: Expr[F2, A1, String])(implicit
    ev: B <:< String
  ): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.MySqlExtensions.SoundsLike)

  def like[F2, A1 <: A](that: Expr[F2, A1, String])(implicit ev: B <:< String): Expr[F with F2, A1, Boolean] =
    Expr.Relational(self, that, RelationalOp.Like)

  def &[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsIntegral[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.AndBit[B1]())

  def |[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsIntegral[B1]): Expr[F with F2, A1, B1] =
    Expr.Binary(self, that, BinaryOp.OrBit[B1]())

  def unary_~[F1 <: F, B1 >: B](implicit ev: IsIntegral[B1]): Expr.Unary[F1, A, B1] =
    Expr.Unary(self, UnaryOp.NotBit[B1]())

  def unary_-[F1 <: F, B1 >: B](implicit ev: IsNumeric[B1]): Expr.Unary[F1, A, B1] =
    Expr.Unary(self, UnaryOp.Negate[B1]())

  def not[F1 <: F, A1 <: A](implicit ev: B <:< Boolean): Expr.Unary[F1, A1, Boolean] =
    Expr.Unary[F1, A1, Boolean](self.widen[Boolean], UnaryOp.NotBool)

  def isNull[A1 <: A]: Expr[F, A1, Boolean] =
    Expr.Property(self, PropertyOp.IsNull)

  def isNotNull[A1 <: A]: Expr[F, A1, Boolean] =
    Expr.Property(self, PropertyOp.IsNotNull)

  def isTrue[A1 <: A](implicit ev: B <:< Boolean): Expr[F, A1, Boolean] =
    Expr.Property(self, PropertyOp.IsTrue)

  def isNotTrue[A1 <: A](implicit ev: B <:< Boolean): Expr[F, A1, Boolean] =
    Expr.Property(self, PropertyOp.IsNotTrue)

  def as[B1 >: B](name: String): Expr[F, A, B1] = {
    val _ = name
    self
  }

  def ascending: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

  def asc: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

  def descending: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

  def desc: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

  def in[F1 <: F, B1 >: B, B2](
    set: Read[B2]
  )(implicit ct: ComparableTypes[B1, B2], ev: Features.IsSource[F1]): Expr[F, A, Boolean] =
    Expr.In(self, set)

  def widen[C](implicit ev: B <:< C): Expr[F, A, C] = {
    val _ = ev
    self.asInstanceOf[Expr[F, A, C]]
  }
}

object Expr {

  sealed trait InvariantExpr[F, -A, B] extends Expr[F, A, B] {
    def typeTag: TypeTag[B]
  }

  def typeTagOf[A](expr: Expr[_, _, A]): TypeTag[A] = expr.asInstanceOf[InvariantExpr[_, _, A]].typeTag

  implicit def literal[A: TypeTag](a: A): Expr[Features.Literal, Any, A] = Expr.Literal(a)

  implicit def some[A: TypeTag.NotNull](someA: Some[A]): Expr[Features.Literal, Any, Option[A]] = {
    implicit val typeTagA: TypeTag.Nullable[A] = TypeTag.Nullable[A]()
    Expr.Literal[Option[A]](someA)
  }

  def exprName[F, A, B](expr: Expr[F, A, B]): Option[String] =
    expr match {
      case Expr.Source(_, Column.Named(name)) => Some(name)
      case _                                  => None
    }

  implicit def expToSelection[F, A, B](
    expr: Expr[F, A, B]
  ): Selection[F, A, SelectionSet.Cons[A, B, SelectionSet.Empty]] =
    Selection.computedOption[F, A, B](expr, Expr.exprName(expr))

  // aggregated F should not be propagated
  final case class Subselect[F <: Features.Aggregated[_], Repr, Source, Subsource, Head](
    subselect: Read.Subselect[F, Repr, _ <: Source, Subsource, Head, SelectionSet.Empty]
  ) extends InvariantExpr[Features.Derived, Any, Head] {
    override def typeTag: TypeTag[Head] = subselect.selection.value.head.toColumn.typeTag
  }

  final case class Source[-A, B, ColumnIdentity, TableType] private[sql] (
    tableName: String,
    column: Column.Aux[B, ColumnIdentity]
  ) extends InvariantExpr[Features.Source[ColumnIdentity, TableType], A, B] {
    def typeTag: TypeTag[B] = column.typeTag
  }

  final case class Unary[-F, -A, B](base: Expr[F, A, B], op: UnaryOp[B]) extends Expr[F, A, B] {
    def typeTag: TypeTag[B] = typeTagOf(base)
  }

  final case class Property[F, -A, +B](base: Expr[F, A, B], op: PropertyOp) extends InvariantExpr[F, A, Boolean] {
    def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
  }

  final case class Binary[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: BinaryOp[B])
      extends InvariantExpr[F1 with F2, A, B] {
    def typeTag: TypeTag[B] = typeTagOf(left)
  }

  final case class Relational[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: RelationalOp)
      extends InvariantExpr[F1 with F2, A, Boolean] {
    def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
  }

  final case class In[F, A, B](value: Expr[F, A, B], set: Read[B]) extends InvariantExpr[F, A, Boolean] {
    def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
  }

  final case class Literal[B: TypeTag](value: B) extends InvariantExpr[Features.Literal, Any, B] {
    def typeTag: TypeTag[B] = implicitly[TypeTag[B]]
  }

  final case class AggregationCall[F, A, B, Z: TypeTag](param: Expr[F, A, B], aggregation: AggregationDef[B, Z])
      extends InvariantExpr[Features.Aggregated[F], A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class ParenlessFunctionCall0[Z: TypeTag](function: FunctionName)
      extends InvariantExpr[Features.Function0, Any, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall0[Z: TypeTag](function: FunctionDef[Any, Z])
      extends InvariantExpr[Features.Function0, Any, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall1[F, A, B, Z: TypeTag](param: Expr[F, A, B], function: FunctionDef[B, Z])
      extends InvariantExpr[F, A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall2[F1, F2, A, B, C, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    function: FunctionDef[(B, C), Z]
  ) extends InvariantExpr[F1 with F2, A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall3[F1, F2, F3, A, B, C, D, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    param3: Expr[F3, A, D],
    function: FunctionDef[(B, C, D), Z]
  ) extends InvariantExpr[F1 with F2 with F3, A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall4[F1, F2, F3, F4, A, B, C, D, E, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    param3: Expr[F3, A, D],
    param4: Expr[F4, A, E],
    function: FunctionDef[(B, C, D, E), Z]
  ) extends InvariantExpr[F1 with F2 with F3 with F4, A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall5[F1, F2, F3, F4, F5, A, B, C, D, E, F, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    param3: Expr[F3, A, D],
    param4: Expr[F4, A, E],
    param5: Expr[F5, A, F],
    function: FunctionDef[(B, C, D, E, F), Z]
  ) extends InvariantExpr[F1 with F2 with F3 with F4 with F5, A, Z] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall6[F1, F2, F3, F4, F5, F6, A, B, C, D, E, F, G, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    param3: Expr[F3, A, D],
    param4: Expr[F4, A, E],
    param5: Expr[F5, A, F],
    param6: Expr[F6, A, G],
    function: FunctionDef[(B, C, D, E, F, G), Z]
  ) extends InvariantExpr[
        F1 with F2 with F3 with F4 with F5 with F6,
        A,
        Z
      ] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }

  final case class FunctionCall7[F1, F2, F3, F4, F5, F6, F7, A, B, C, D, E, F, G, H, Z: TypeTag](
    param1: Expr[F1, A, B],
    param2: Expr[F2, A, C],
    param3: Expr[F3, A, D],
    param4: Expr[F4, A, E],
    param5: Expr[F5, A, F],
    param6: Expr[F6, A, G],
    param7: Expr[F7, A, H],
    function: FunctionDef[(B, C, D, E, F, G, H), Z]
  ) extends InvariantExpr[
        F1 with F2 with F3 with F4 with F5 with F6 with F7,
        A,
        Z
      ] {
    def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
  }
}
