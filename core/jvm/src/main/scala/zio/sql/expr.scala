package zio.sql

import com.github.ghik.silencer.silent

import java.time._
import scala.language.implicitConversions
import java.math.BigDecimal
import scala.annotation.implicitNotFound

trait ExprModule extends NewtypesModule with OpsModule {
  self: SelectModule with TableModule =>

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

    implicit val subqueryToExpr = self.Read.Subselect.subselectToExpr _

    sealed trait InvariantExpr[F, -A, B] extends Expr[F, A, B] {
      def typeTag: TypeTag[B]
    }

    def typeTagOf[A](expr: Expr[_, _, A]): TypeTag[A] = expr.asInstanceOf[InvariantExpr[_, _, A]].typeTag

    implicit def literal[A: TypeTag](a: A): Expr[Features.Literal, Any, A] = Expr.Literal(a)

    implicit def some[A: TypeTag.NotNull](someA: Some[A]): Expr[Features.Literal, Any, Option[A]] = {
      implicit val typeTagA = TypeTag.Nullable[A]()
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
    sealed case class Subselect[F <: Features.Aggregated[_], Repr, Source, Subsource, Head](
      subselect: Read.Subselect[F, Repr, _ <: Source, Subsource, Head, SelectionSet.Empty]
    ) extends InvariantExpr[Features.Derived, Any, Head] {
      override def typeTag: TypeTag[Head] = subselect.selection.value.head.toColumn.typeTag
    }

    sealed case class Source[-A, B, ColumnIdentity, TableType] private[sql] (
      tableName: TableName,
      column: Column.Aux[B, ColumnIdentity]
    ) extends InvariantExpr[Features.Source[ColumnIdentity, TableType], A, B] {
      def typeTag: TypeTag[B] = column.typeTag
    }

    sealed case class Unary[-F, -A, B](base: Expr[F, A, B], op: UnaryOp[B]) extends Expr[F, A, B] {
      def typeTag: TypeTag[B] = typeTagOf(base)
    }

    sealed case class Property[F, -A, +B](base: Expr[F, A, B], op: PropertyOp) extends InvariantExpr[F, A, Boolean] {
      def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
    }

    sealed case class Binary[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: BinaryOp[B])
        extends InvariantExpr[F1 with F2, A, B] {
      def typeTag: TypeTag[B] = typeTagOf(left)
    }

    sealed case class Relational[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: RelationalOp)
        extends InvariantExpr[F1 with F2, A, Boolean] {
      def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
    }

    sealed case class In[F, A, B](value: Expr[F, A, B], set: Read[B]) extends InvariantExpr[F, A, Boolean] {
      def typeTag: TypeTag[Boolean] = TypeTag.TBoolean
    }

    sealed case class Literal[B: TypeTag](value: B) extends InvariantExpr[Features.Literal, Any, B] {
      def typeTag: TypeTag[B] = implicitly[TypeTag[B]]
    }

    sealed case class AggregationCall[F, A, B, Z: TypeTag](param: Expr[F, A, B], aggregation: AggregationDef[B, Z])
        extends InvariantExpr[Features.Aggregated[F], A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class ParenlessFunctionCall0[Z: TypeTag](function: FunctionName)
        extends InvariantExpr[Features.Function0, Any, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall0[Z: TypeTag](function: FunctionDef[Any, Z])
        extends InvariantExpr[Features.Function0, Any, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall1[F, A, B, Z: TypeTag](param: Expr[F, A, B], function: FunctionDef[B, Z])
        extends InvariantExpr[F, A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall2[F1, F2, A, B, C, Z: TypeTag](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      function: FunctionDef[(B, C), Z]
    ) extends InvariantExpr[F1 with F2, A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall3[F1, F2, F3, A, B, C, D, Z: TypeTag](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      param3: Expr[F3, A, D],
      function: FunctionDef[(B, C, D), Z]
    ) extends InvariantExpr[F1 with F2 with F3, A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall4[F1, F2, F3, F4, A, B, C, D, E, Z: TypeTag](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      param3: Expr[F3, A, D],
      param4: Expr[F4, A, E],
      function: FunctionDef[(B, C, D, E), Z]
    ) extends InvariantExpr[F1 with F2 with F3 with F4, A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall5[F1, F2, F3, F4, F5, A, B, C, D, E, F, Z: TypeTag](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      param3: Expr[F3, A, D],
      param4: Expr[F4, A, E],
      param5: Expr[F5, A, F],
      function: FunctionDef[(B, C, D, E, F), Z]
    ) extends InvariantExpr[F1 with F2 with F3 with F4 with F5, A, Z] {
      def typeTag: TypeTag[Z] = implicitly[TypeTag[Z]]
    }

    sealed case class FunctionCall6[F1, F2, F3, F4, F5, F6, A, B, C, D, E, F, G, Z: TypeTag](
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

    sealed case class FunctionCall7[F1, F2, F3, F4, F5, F6, F7, A, B, C, D, E, F, G, H, Z: TypeTag](
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

  sealed case class AggregationDef[-A, +B](name: FunctionName) { self =>

    def apply[F, Source, B1 >: B](expr: Expr[F, Source, A])(implicit
      typeTag: TypeTag[B1]
    ): Expr[Features.Aggregated[F], Source, B1] =
      Expr.AggregationCall[F, Source, A, B1](expr, self)
  }

  object AggregationDef {
    val Count                                      = AggregationDef[Any, Long](FunctionName("count"))
    val Sum                                        = AggregationDef[Double, Double](FunctionName("sum"))
    val SumInt                                     = AggregationDef[Int, Int](FunctionName("sum"))
    val SumDec                                     = AggregationDef[BigDecimal, BigDecimal](FunctionName("sum"))
    val Avg                                        = AggregationDef[Double, Double](FunctionName("avg"))
    val AvgDec                                     = AggregationDef[BigDecimal, BigDecimal](FunctionName("avg"))
    def Min[F, A, B: TypeTag](expr: Expr[F, A, B]) = AggregationDef[B, B](FunctionName("min"))(expr)
    def Max[F, A, B: TypeTag](expr: Expr[F, A, B]) = AggregationDef[B, B](FunctionName("max"))(expr)
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName) { self =>

    def apply[B1 >: B]()(implicit ev: Any <:< A, typeTag: TypeTag[B1]): Expr[Features.Function0, Any, B1] =
      Expr.FunctionCall0(self.asInstanceOf[FunctionDef[Any, B1]])

    def apply[F, Source, B1 >: B](param1: Expr[F, Source, A])(implicit typeTag: TypeTag[B1]): Expr[F, Source, B1] =
      Expr.FunctionCall1(param1, self: FunctionDef[A, B1])

    def apply[F1, F2, Source, P1, P2, B1 >: B](param1: Expr[F1, Source, P1], param2: Expr[F2, Source, P2])(implicit
      ev: (P1, P2) <:< A,
      typeTag: TypeTag[B1]
    ): Expr[F1 with F2, Source, B1] =
      Expr.FunctionCall2(param1, param2, self.narrow[(P1, P2)]: FunctionDef[(P1, P2), B1])

    def apply[F1, F2, F3, Source, P1, P2, P3, B1 >: B](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3]
    )(implicit ev: (P1, P2, P3) <:< A, typeTag: TypeTag[B1]): Expr[F1 with F2 with F3, Source, B1] =
      Expr.FunctionCall3(param1, param2, param3, self.narrow[(P1, P2, P3)]: FunctionDef[(P1, P2, P3), B1])

    def apply[F1, F2, F3, F4, Source, P1, P2, P3, P4, B1 >: B](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3],
      param4: Expr[F4, Source, P4]
    )(implicit ev: (P1, P2, P3, P4) <:< A, typeTag: TypeTag[B1]): Expr[F1 with F2 with F3 with F4, Source, B1] =
      Expr.FunctionCall4(
        param1,
        param2,
        param3,
        param4,
        self.narrow[(P1, P2, P3, P4)]: FunctionDef[(P1, P2, P3, P4), B1]
      )

    def apply[F1, F2, F3, F4, F5, Source, P1, P2, P3, P4, P5, B1 >: B](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3],
      param4: Expr[F4, Source, P4],
      param5: Expr[F5, Source, P5]
    )(implicit
      ev: (P1, P2, P3, P4, P5) <:< A,
      typeTag: TypeTag[B1]
    ): Expr[F1 with F2 with F3 with F4 with F5, Source, B1] =
      Expr.FunctionCall5(
        param1,
        param2,
        param3,
        param4,
        param5,
        self.narrow[(P1, P2, P3, P4, P5)]: FunctionDef[(P1, P2, P3, P4, P5), B1]
      )

    def apply[F1, F2, F3, F4, F5, F6, Source, P1, P2, P3, P4, P5, P6, B1 >: B](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3],
      param4: Expr[F4, Source, P4],
      param5: Expr[F5, Source, P5],
      param6: Expr[F6, Source, P6]
    )(implicit
      ev: (P1, P2, P3, P4, P5, P6) <:< A,
      typeTag: TypeTag[B1]
    ): Expr[F1 with F2 with F3 with F4 with F5 with F6, Source, B1] =
      Expr.FunctionCall6(
        param1,
        param2,
        param3,
        param4,
        param5,
        param6,
        self.narrow[(P1, P2, P3, P4, P5, P6)]: FunctionDef[(P1, P2, P3, P4, P5, P6), B1]
      )

    def apply[F1, F2, F3, F4, F5, F6, F7, Source, P1, P2, P3, P4, P5, P6, P7, B1 >: B](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3],
      param4: Expr[F4, Source, P4],
      param5: Expr[F5, Source, P5],
      param6: Expr[F6, Source, P6],
      param7: Expr[F7, Source, P7]
    )(implicit
      ev: (P1, P2, P3, P4, P5, P6, P7) <:< A,
      typeTag: TypeTag[B1]
    ): Expr[F1 with F2 with F3 with F4 with F5 with F6 with F7, Source, B1] =
      Expr.FunctionCall7(
        param1,
        param2,
        param3,
        param4,
        param5,
        param6,
        param7,
        self.narrow[(P1, P2, P3, P4, P5, P6, P7)]: FunctionDef[(P1, P2, P3, P4, P5, P6, P7), B1]
      )

    def narrow[C](implicit ev: C <:< A): FunctionDef[C, B] = {
      val _ = ev
      self.asInstanceOf[FunctionDef[C, B]]
    }
  }

  object FunctionDef {

    // math functions
    val Abs         = FunctionDef[Double, Double](FunctionName("abs"))
    val Acos        = FunctionDef[Double, Double](FunctionName("acos"))
    val Asin        = FunctionDef[Double, Double](FunctionName("asin"))
    val Atan        = FunctionDef[Double, Double](FunctionName("atan"))
    val Ceil        = FunctionDef[Double, Double](FunctionName("ceil"))
    val Cos         = FunctionDef[Double, Double](FunctionName("cos"))
    val Exp         = FunctionDef[Double, Double](FunctionName("exp"))
    val Floor       = FunctionDef[Double, Double](FunctionName("floor"))
    val Ln          = FunctionDef[Double, Double](FunctionName("ln"))
    val Log         = FunctionDef[(Double, Double), Double](FunctionName("log"))
    val Mod         = FunctionDef[(Double, Double), Double](FunctionName("mod"))
    val Power       = FunctionDef[(Double, Double), Double](FunctionName("power"))
    val Round       = FunctionDef[(Double, Int), Double](FunctionName("round"))
    val Sign        = FunctionDef[Double, Int](FunctionName("sign"))
    val Sin         = FunctionDef[Double, Double](FunctionName("sin"))
    val Sqrt        = FunctionDef[Double, Double](FunctionName("sqrt"))
    val Tan         = FunctionDef[Double, Double](FunctionName("tan"))
    val WidthBucket = FunctionDef[(Double, Double, Double, Int), Int](FunctionName("width_bucket"))

    // string functions
    val Ascii       = FunctionDef[String, Int](FunctionName("ascii"))
    val CharLength  = FunctionDef[String, Int](FunctionName("character_length"))
    val Concat      = FunctionDef[(String, String), String](FunctionName("concat")) // todo varargs
    val ConcatWs3   = FunctionDef[(String, String, String), String](FunctionName("concat_ws"))
    val ConcatWs4   = FunctionDef[(String, String, String, String), String](FunctionName("concat_ws"))
    val Lower       = FunctionDef[String, String](FunctionName("lower"))
    val Ltrim       = FunctionDef[String, String](FunctionName("ltrim"))
    val OctetLength = FunctionDef[String, Int](FunctionName("octet_length"))
    val Overlay     = FunctionDef[(String, String, Int, Option[Int]), String](FunctionName("overlay"))
    val Position    = FunctionDef[(String, String), Int](FunctionName("position"))
    val Replace     = FunctionDef[(String, String, String), String](FunctionName("replace"))
    val Rtrim       = FunctionDef[String, String](FunctionName("rtrim"))
    val Substring   = FunctionDef[(String, Int, Option[Int]), String](FunctionName("substring"))
    // TODO substring regex
    val Trim        = FunctionDef[String, String](FunctionName("trim"))
    val Upper       = FunctionDef[String, String](FunctionName("upper"))

    // date functions
    val CurrentTimestamp = FunctionDef[Nothing, Instant](FunctionName("current_timestamp"))
  }

  sealed trait Set[F, -A] {
    type Value

    def lhs: Expr[F, A, Value]
    def rhs: Expr[_, A, Value]

    def typeTag: TypeTag[Value]

  }

  object Set {
    type Aux[F, -A, Value0] = Set[F, A] { type Value = Value0 }

    @silent
    def apply[F: Features.IsSource, A, Value0: TypeTag](
      lhs0: Expr[F, A, Value0],
      rhs0: Expr[_, A, Value0]
    ): Set.Aux[F, A, Value0] =
      new Set[F, A] {
        type Value = Value0

        def lhs = lhs0
        def rhs = rhs0

        def typeTag = implicitly[TypeTag[Value]]
      }
  }

  @implicitNotFound(
    "You cannot compare values of different types ${A} and ${B}. " +
      "As those are unrelated types, this query would fail at database level."
  )
  sealed trait ComparableTypes[A, B]

  object ComparableTypes extends ComparableTypesLowPriority {
    implicit final def comparableSubtype1[A <: B, B]: ComparableTypes[A, B] = new ComparableTypes[A, B] {}

    implicit final def AWithOptionIsComparable[A]: ComparableTypes[A, Option[A]] = new ComparableTypes[A, Option[A]] {}
    implicit final def optionWithAIsComparable[A]: ComparableTypes[Option[A], A] = new ComparableTypes[Option[A], A] {}

    implicit final def optionAndNone[A]: ComparableTypes[Option[A], None.type] =
      new ComparableTypes[Option[A], None.type] {}
    implicit final def noneAndOption[A]: ComparableTypes[None.type, Option[A]] =
      new ComparableTypes[None.type, Option[A]] {}

    implicit final def optionAndSome[A]: ComparableTypes[Option[A], Expr.Literal[Some[A]]] =
      new ComparableTypes[Option[A], Expr.Literal[Some[A]]] {}
    implicit final def someAndOption[A]: ComparableTypes[Expr.Literal[Some[A]], Option[A]] =
      new ComparableTypes[Expr.Literal[Some[A]], Option[A]] {}

    implicit final def dateIsComprable[A, B](implicit ev1: IsDate[A], ev2: IsDate[B]): ComparableTypes[A, B] =
      new ComparableTypes[A, B] {}

    implicit final def numericIsComparable[A, B](implicit
      ev1: IsNumeric[A],
      ev2: IsNumeric[B]
    ): ComparableTypes[A, B] = new ComparableTypes[A, B] {}
  }

  sealed trait ComparableTypesLowPriority {
    implicit final def comparableSubtype2[A, B <: A]: ComparableTypes[A, B] = new ComparableTypes[A, B] {}
  }
}
