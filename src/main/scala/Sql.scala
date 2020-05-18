package zio.sql

import scala.language.implicitConversions

import java.time._
import scala.annotation.implicitNotFound

trait Sql {
  type ColumnName = String
  type TableName  = String

  type TypeTagExtension[+A]

  sealed trait TypeTag[A]

  object TypeTag {
    implicit case object TBigDecimal                                             extends TypeTag[BigDecimal]
    implicit case object TBoolean                                                extends TypeTag[Boolean]
    implicit case object TByteArray                                              extends TypeTag[Array[Byte]]
    implicit case object TChar                                                   extends TypeTag[Char]
    implicit case object TDouble                                                 extends TypeTag[Double]
    implicit case object TFloat                                                  extends TypeTag[Float]
    implicit case object TInstant                                                extends TypeTag[Instant]
    implicit case object TInt                                                    extends TypeTag[Int]
    implicit case object TLocalDate                                              extends TypeTag[LocalDate]
    implicit case object TLocalDateTime                                          extends TypeTag[LocalDateTime]
    implicit case object TLocalTime                                              extends TypeTag[LocalTime]
    implicit case object TLong                                                   extends TypeTag[Long]
    implicit case object TOffsetDateTime                                         extends TypeTag[OffsetDateTime]
    implicit case object TOffsetTime                                             extends TypeTag[OffsetTime]
    implicit case object TShort                                                  extends TypeTag[Short]
    implicit case object TString                                                 extends TypeTag[String]
    implicit case object TZonedDateTime                                          extends TypeTag[ZonedDateTime]
    sealed case class TDialectSpecific[A](typeTagExtension: TypeTagExtension[A]) extends TypeTag[A]

    sealed case class TOption[A: TypeTag]() extends TypeTag[Option[A]] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }

    implicit def option[A: TypeTag]: TypeTag[Option[A]] = TOption[A]

    implicit def dialectSpecific[A](implicit typeTagExtension: TypeTagExtension[A]): TypeTag[A] =
      TDialectSpecific(typeTagExtension)
  }

  sealed trait IsNumeric[A] {
    def typeTag: TypeTag[A]
  }

  object IsNumeric {

    abstract class AbstractIsNumeric[A: TypeTag] extends IsNumeric[A] {
      def typeTag = implicitly[TypeTag[A]]
    }
    implicit case object TShortIsNumeric  extends AbstractIsNumeric[Short]
    implicit case object TIntIsNumeric    extends AbstractIsNumeric[Int]
    implicit case object TLongIsNumeric   extends AbstractIsNumeric[Long]
    implicit case object TFloatIsNumeric  extends AbstractIsNumeric[Float]
    implicit case object TDoubleIsNumeric extends AbstractIsNumeric[Double]
  }

  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]
    type Append[That <: ColumnSet] <: ColumnSet

    def ++ [That <: ColumnSet](that: That): Append[That]

    def columnsUntyped: List[Column.Untyped]

    protected def mkColumns[T](name: TableName): ColumnsRepr[T]
  }

  object ColumnSet {
    type Empty                  = Empty.type
    type :*:[A, B <: ColumnSet] = Cons[A, B]
    type Singleton[A]           = Cons[A, Empty]

    case object Empty extends ColumnSet {
      type ColumnsRepr[_]            = Unit
      type Append[That <: ColumnSet] = That

      override def ++ [That <: ColumnSet](that: That): Append[That] = that

      override def columnsUntyped: List[Column.Untyped] = Nil

      override protected def mkColumns[T](name: TableName): ColumnsRepr[T] = ()
    }

    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>
      type ColumnsRepr[T]            = (Expr[Any, T, A], tail.ColumnsRepr[T])
      type Append[That <: ColumnSet] = Cons[A, tail.Append[That]]

      override def ++ [That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)

      override def columnsUntyped: List[Column.Untyped] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Source[ColumnsRepr, A :*: B] =
        new Table.Source[ColumnsRepr, A :*: B] {
          val name: TableName                      = name0
          val columnSchema: ColumnSchema[A :*: B]  = ColumnSchema(self)
          val columns: ColumnsRepr[TableType]      = mkColumns[TableType](name0)
          val columnsUntyped: List[Column.Untyped] = self.columnsUntyped
        }

      override protected def mkColumns[T](name: TableName): ColumnsRepr[T] =
        (Expr.Source(name, head), tail.mkColumns(name))
    }

    def bigDecimal(name: String): Singleton[BigDecimal]         = singleton[BigDecimal](name)
    def boolean(name: String): Singleton[Boolean]               = singleton[Boolean](name)
    def byteArray(name: String): Singleton[Array[Byte]]         = singleton[Array[Byte]](name)
    def char(name: String): Singleton[Char]                     = singleton[Char](name)
    def double(name: String): Singleton[Double]                 = singleton[Double](name)
    def float(name: String): Singleton[Float]                   = singleton[Float](name)
    def instant(name: String): Singleton[Instant]               = singleton[Instant](name)
    def int(name: String): Singleton[Int]                       = singleton[Int](name)
    def localDate(name: String): Singleton[LocalDate]           = singleton[LocalDate](name)
    def localDateTime(name: String): Singleton[LocalDateTime]   = singleton[LocalDateTime](name)
    def localTime(name: String): Singleton[LocalTime]           = singleton[LocalTime](name)
    def long(name: String): Singleton[Long]                     = singleton[Long](name)
    def offsetDateTime(name: String): Singleton[OffsetDateTime] = singleton[OffsetDateTime](name)
    def offsetTime(name: String): Singleton[OffsetTime]         = singleton[OffsetTime](name)
    def short(name: String): Singleton[Short]                   = singleton[Short](name)
    def singleton[A: TypeTag](name: String): Singleton[A]       = Cons(Column[A](name), Empty)
    def string(name: String): Singleton[String]                 = singleton[String](name)
    def zonedDateTime(name: String): Singleton[ZonedDateTime]   = singleton[ZonedDateTime](name)
  }

  object :*: {
    def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
  }

  sealed case class FunctionName(name: String)

  sealed case class Column[A: TypeTag](name: String) {
    def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
  }

  object Column {
    type Untyped = Column[_]
  }

  sealed trait JoinType

  object JoinType {
    case object Inner      extends JoinType
    case object LeftOuter  extends JoinType
    case object RightOuter extends JoinType
    case object FullOuter  extends JoinType
  }

  /**
   * (left join right) on (...)
   */
  sealed trait Table { self =>
    type TableType

    final def fullOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.FullOuter, self, that)

    final def join[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.Inner, self, that)

    final def leftOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.LeftOuter, self, that)

    final def rightOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.RightOuter, self, that)
  }

  object Table {

    class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {

      def on(expr: Expr[Any, A with B, Boolean]): Table.Aux[A with B] =
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    sealed trait Source[F[_], A] extends Table {
      val name: TableName
      val columnSchema: ColumnSchema[A]
      val columns: F[TableType]
      val columnsUntyped: List[Column.Untyped]
    }

    sealed case class Joined[A, B](
      joinType: JoinType,
      left: Table.Aux[A],
      right: Table.Aux[B],
      on: Expr[Any, A with B, Boolean]
    ) extends Table {
      type TableType = left.TableType with right.TableType
    }
  }

  /*
   * (SELECT *, "foo", table.a + table.b AS sum... FROM table WHERE cond) UNION (SELECT ... FROM table)
   *   UNION ('1', '2', '3')
   *   ORDER BY table.a ASC, foo, sum DESC
   *   LIMIT 200
   *   OFFSET 100
   * UPDATE table SET ...
   * INSERT ... INTO table
   * DELETE ... FROM table
   *
   * SELECT ARBITRARY(age), COUNT(*) FROM person GROUP BY age
   */
  def select[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SelectBuilder[F, A, B] =
    SelectBuilder(selection)

  sealed case class SelectBuilder[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) {

    def from(table: Table.Aux[A]): Read.Select[F, A, B] =
      Read.Select(selection, table, true, Nil)
  }

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+A] { self =>
    def union[A1 >: A](that: Read[A1]): Read[A1] = Read.Union(self, that, true)

    def unionAll[A1 >: A](that: Read[A1]): Read[A1] = Read.Union(self, that, false)
  }

  object Read {

    sealed case class Select[+F, A, B <: SelectionSet[A]](
      selection: Selection[F, A, B],
      table: Table.Aux[A],
      whereExpr: Expr[Any, A, Boolean],
      groupBy: List[Expr[Any, A, Any]],
      orderBy: List[Ordering[Expr[Any, A, Any]]] = Nil,
      offset: Option[Long] = None,
      limit: Option[Long] = None
    ) extends Read[B] { self =>

      def where(whereExpr2: Expr[Any, A, Boolean]): Select[F, A, B] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Select[F, A, B] = copy(limit = Some(n))

      def offset(n: Long): Select[F, A, B] = copy(offset = Some(n))

      def orderBy(o: Ordering[Expr[Any, A, Any]], os: Ordering[Expr[Any, A, Any]]*): Select[F, A, B] =
        copy(orderBy = self.orderBy ++ (o :: os.toList))

      def groupBy[F1 >: F](key: Expr[F1, A, Any], keys: Expr[F1, A, Any]*)(
        implicit @implicitNotFound(
          "You must aggregate all columns in the selection in order to call GROUP BY. Please see Arbitrary aggregation for group keys"
        ) ev: F <:< Features.Aggregated
      ): Select[F1, A, B] = {
        val _ = ev
        copy(groupBy = groupBy ++ (key :: keys.toList))
      }
    }

    sealed case class Union[B](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B]

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[B]

    def lit[B: TypeTag](values: B*): Read[B] = Literal(values.toSeq)
  }

  sealed trait Ordering[+A]

  object Ordering {
    sealed case class Asc[A](value: A)  extends Ordering[A]
    sealed case class Desc[A](value: A) extends Ordering[A]

    implicit def exprToOrdering[F, A, B](expr: Expr[F, A, B]): Ordering[Expr[F, A, B]] =
      Asc(expr)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[+F, -A, +B <: SelectionSet[A]](value: B) { self =>
    type SelectionType

    def ++ [F1 >: F, A1 <: A, C <: SelectionSet[A1]](
      that: Selection[F1, A1, C]
    ): Selection[F1, A1, self.value.Append[A1, C]] =
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

    val selection =
      computed(FunctionDef.CharLength(Expr.Literal("test"))) ++
        constant(1) ++ empty ++ constant("foo") ++ constant(true) ++ empty

    val int :*: str :*: bool :*: _ = selection.columns
  }

  sealed trait ColumnSelection[-A, +B] {
    def name: Option[ColumnName]
  }

  object ColumnSelection {
    sealed case class Constant[A: TypeTag](value: A, name: Option[ColumnName])         extends ColumnSelection[Any, A]
    sealed case class Computed[F, A, B](expr: Expr[F, A, B], name: Option[ColumnName]) extends ColumnSelection[A, B]
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[Source1, T]

    type Append[Source1, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    def ++ [Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
  }

  object SelectionSet {
    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {
      override type SelectionsRepr[Source1, T] = Unit

      override type Append[Source1, That <: SelectionSet[Source1]] = That

      override def ++ [Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
    }

    sealed case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
        extends SelectionSet[Source] { self =>
      override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

      override type Append[Source1, That <: SelectionSet[Source1]] =
        Cons[Source1, A, tail.Append[Source1, That]]

      override def ++ [Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
    }
  }

  sealed trait BinaryOp[A]

  object BinaryOp {

    sealed case class Add[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
    }

    sealed case class Sub[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
    }

    sealed case class Mul[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
    }

    sealed case class Div[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
    }
    case object DivInt       extends BinaryOp[Int]
    case object ModInt       extends BinaryOp[Int]
    case object DivLong      extends BinaryOp[Long]
    case object ModLong      extends BinaryOp[Long]
    case object AndBool      extends BinaryOp[Boolean]
    case object OrBool       extends BinaryOp[Boolean]
    case object StringConcat extends BinaryOp[String]
  }
  sealed trait RelationalOp

  object RelationalOp {
    case object Equals           extends RelationalOp
    case object LessThan         extends RelationalOp
    case object GreaterThan      extends RelationalOp
    case object LessThanEqual    extends RelationalOp
    case object GreaterThanEqual extends RelationalOp
  }

  object Features {
    type Aggregated
  }

  /**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
  sealed trait Expr[+F, -A, +B] { self =>

    def + [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F1, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Add[B1]())

    def - [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F1, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Sub[B1]())

    def * [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F1, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Sub[B1]())

    def && [F1 >: F, A1 <: A](that: Expr[F1, A1, Boolean])(implicit ev: B <:< Boolean): Expr[F1, A1, Boolean] =
      Expr.Binary(self.widen[Boolean], that, BinaryOp.AndBool)

    def || [F1 >: F, A1 <: A](that: Expr[F1, A1, Boolean])(implicit ev: B <:< Boolean): Expr[F1, A1, Boolean] =
      Expr.Binary(self.widen[Boolean], that, BinaryOp.OrBool)

    def === [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1]): Expr[F1, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.Equals)

    def > [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1]): Expr[F1, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.GreaterThan)

    def < [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1]): Expr[F1, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.LessThan)

    def >= [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1]): Expr[F1, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.GreaterThanEqual)

    def <= [F1 >: F, A1 <: A, B1 >: B](that: Expr[F1, A1, B1]): Expr[F1, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.LessThanEqual)

    def as[B1 >: B](name: String): Selection[F, A, SelectionSet.Cons[A, B1, SelectionSet.Empty]] =
      Selection.computedAs(self, name)

    def ascending: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

    def asc: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

    def descending: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

    def desc: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

    def in[B1 >: B](set: Read[B1]): Expr[F, A, Boolean] = Expr.In(self, set)

    def widen[C](implicit ev: B <:< C): Expr[F, A, C] = {
      val _ = ev

      self.asInstanceOf[Expr[F, A, C]]
    }
  }

  object Expr {
    implicit def literal[A: TypeTag](a: A): Expr[Any, Any, A] = Expr.Literal(a)

    sealed case class Source[A, B] private[Sql] (tableName: TableName, column: Column[B])         extends Expr[Any, A, B]
    sealed case class Binary[F, A, B](left: Expr[F, A, B], right: Expr[F, A, B], op: BinaryOp[B]) extends Expr[F, A, B]

    sealed case class Relational[F, A, B](left: Expr[F, A, B], right: Expr[F, A, B], op: RelationalOp)
        extends Expr[F, A, Boolean]
    sealed case class In[F, A, B](value: Expr[F, A, B], set: Read[B]) extends Expr[F, A, Boolean]
    sealed case class Literal[B: TypeTag](value: B)                   extends Expr[Any, Any, B]

    sealed case class AggregationCall[F, A, B, Z](param: Expr[F, A, B], aggregation: AggregationDef[B, Z])
        extends Expr[F with Features.Aggregated, A, Z]

    sealed case class FunctionCall1[F, A, B, Z](param: Expr[F, A, B], function: FunctionDef[B, Z]) extends Expr[F, A, Z]

    sealed case class FunctionCall2[F, A, B, C, Z](
      param1: Expr[F, A, B],
      param2: Expr[F, A, C],
      function: FunctionDef[(B, C), Z]
    ) extends Expr[F, A, Z]

    sealed case class FunctionCall3[F, A, B, C, D, Z](
      param1: Expr[F, A, B],
      param2: Expr[F, A, C],
      param3: Expr[F, A, D],
      function: FunctionDef[(B, C, D), Z]
    ) extends Expr[F, A, Z]

    sealed case class FunctionCall4[F, A, B, C, D, E, Z](
      param1: Expr[F, A, B],
      param2: Expr[F, A, C],
      param3: Expr[F, A, D],
      param4: Expr[F, A, E],
      function: FunctionDef[(B, C, D, E), Z]
    ) extends Expr[F, A, Z]
  }

  sealed case class AggregationDef[-A, +B](name: FunctionName) { self =>

    def apply[F, Source](expr: Expr[F, Source, A]): Expr[F with Features.Aggregated, Source, B] =
      Expr.AggregationCall[F, Source, A, B](expr, self)
  }

  sealed case class AggregationDefPoly[-Caps[_], +B](name: FunctionName) {
    def apply[A: Caps, Source](a: Expr[Any, Source, A]): Expr[Features.Aggregated, Source, B] = ???
  }

  object AggregationDef {
    val Count     = AggregationDef[Any, Long](FunctionName("count"))
    val Sum       = AggregationDef[Double, Double](FunctionName("sum"))
    val Arbitrary = AggregationDef[Any, Any](FunctionName("arbitrary"))
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName) { self =>
    def apply[F, Source](param1: Expr[F, Source, A]): Expr[F, Source, B] = Expr.FunctionCall1(param1, self)

    def apply[F, A1 <: A, Source, P1, P2](param1: Expr[F, Source, P1], param2: Expr[F, Source, P2])(
      implicit ev: A1 =:= (P1, P2)
    ): Expr[F, Source, B] =
      Expr.FunctionCall2(param1, param2, (self: FunctionDef[A1, B]).narrow[A1, (P1, P2)])

    def apply[F, A1 <: A, Source, P1, P2, P3](
      param1: Expr[F, Source, P1],
      param2: Expr[F, Source, P2],
      param3: Expr[F, Source, P3]
    )(implicit ev: A1 =:= (P1, P2, P3)): Expr[F, Source, B] =
      Expr.FunctionCall3(param1, param2, param3, (self: FunctionDef[A1, B]).narrow[A1, (P1, P2, P3)])

    def apply[F, A1 <: A, Source, P1, P2, P3, P4](
      param1: Expr[F, Source, P1],
      param2: Expr[F, Source, P2],
      param3: Expr[F, Source, P3],
      param4: Expr[F, Source, P4]
    )(implicit ev: A1 =:= (P1, P2, P3, P4)): Expr[F, Source, B] =
      Expr.FunctionCall4(param1, param2, param3, param4, (self: FunctionDef[A1, B]).narrow[A1, (P1, P2, P3, P4)])

    def narrow[A1 <: A, C](implicit ev: A1 =:= C): FunctionDef[C, B] = {
      val _ = ev

      self.asInstanceOf[FunctionDef[C, B]]
    }
  }

  object FunctionDef {
    //match functions
    val Abs   = FunctionDef[Double, Double](FunctionName("abs"))
    val Ceil  = FunctionDef[Double, Double](FunctionName("ceil"))
    val Exp   = FunctionDef[Double, Double](FunctionName("exp"))
    val Floor = FunctionDef[Double, Double](FunctionName("floor"))
    //val Log = FunctionDef[Double, Double](FunctionName("log")) //not part of SQL 2011 spec
    val Ln          = FunctionDef[Double, Double](FunctionName("ln"))
    val Mod         = FunctionDef[(Double, Double), Double](FunctionName("mod"))
    val Power       = FunctionDef[(Double, Double), Double](FunctionName("power"))
    val Sqrt        = FunctionDef[Double, Double](FunctionName("sqrt"))
    val WidthBucket = FunctionDef[(Double, Double, Double, Int), Int](FunctionName("width bucket"))

    //string functions
    val CharLength  = FunctionDef[String, Int](FunctionName("character length"))
    val Lower       = FunctionDef[String, String](FunctionName("lower"))
    val OctetLength = FunctionDef[String, Int](FunctionName("octet length"))
    val Overlay     = FunctionDef[(String, String, Int, Option[Int]), String](FunctionName("overlay"))
    val Position    = FunctionDef[(String, String), Int](FunctionName("position"))
    val Substring   = FunctionDef[(String, Int, Option[Int]), String](FunctionName("substring"))
    //TODO substring regex
    val Trim  = FunctionDef[String, String](FunctionName("trim"))
    val Upper = FunctionDef[String, String](FunctionName("upper"))
  }

  object Example1 {
    import ColumnSet._

    val columnSet = int("age") ++ string("name")

    val table = columnSet.table("person")

    val table2 = columnSet.table("person2")

    val age :*: name :*: _ = table.columns

    val age2 :*: name2 :*: _ = table2.columns

    val joined = (table join table2) on (age === age2)

    import FunctionDef._
    import AggregationDef._

    val query1 =
      (select {
        ((age + 2) as "age") ++ (name as "name") ++ (Abs(3.0) as "dummy")
      } from table).limit(200).offset(1000).orderBy(age.descending)

    val query2 =
      (select {
        (Arbitrary(age) as "age") ++ (Count(name) as "count")
      } from table).groupBy(age)
  }
}
