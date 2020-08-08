package zio.sql

import scala.language.implicitConversions

import java.time._
import java.util.UUID

import scala.annotation.implicitNotFound

trait Sql {
  type ColumnName = String
  type TableName  = String

  type TypeTagExtension[+A]

  sealed trait TypeTag[A]

  object TypeTag {
    implicit case object TBigDecimal                                             extends TypeTag[BigDecimal]
    implicit case object TBoolean                                                extends TypeTag[Boolean]
    implicit case object TByte                                                   extends TypeTag[Byte]
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
    implicit case object TUUID                                                   extends TypeTag[UUID]
    implicit case object TZonedDateTime                                          extends TypeTag[ZonedDateTime]
    sealed case class TDialectSpecific[A](typeTagExtension: TypeTagExtension[A]) extends TypeTag[A]

    sealed case class TOption[A: TypeTag]() extends TypeTag[Option[A]] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }

    implicit def option[A: TypeTag]: TypeTag[Option[A]] = TOption[A]

    implicit def dialectSpecific[A](implicit typeTagExtension: TypeTagExtension[A]): TypeTag[A] =
      TDialectSpecific(typeTagExtension)
  }

  sealed trait IsIntegral[A] {
    def typeTag: TypeTag[A]
  }

  object IsIntegral {

    abstract class AbstractIsIntegral[A: TypeTag] extends IsIntegral[A] {
      def typeTag = implicitly[TypeTag[A]]
    }
    implicit case object TByteIsIntegral  extends AbstractIsIntegral[Byte]
    implicit case object TShortIsIntegral extends AbstractIsIntegral[Short]
    implicit case object TIntIsIntegral   extends AbstractIsIntegral[Int]
    implicit case object TLongIsIntegral  extends AbstractIsIntegral[Long]
  }

  sealed trait IsNumeric[A] {
    def typeTag: TypeTag[A]
  }

  object IsNumeric {

    abstract class AbstractIsNumeric[A: TypeTag] extends IsNumeric[A] {
      def typeTag = implicitly[TypeTag[A]]
    }
    implicit case object TShortIsNumeric      extends AbstractIsNumeric[Short]
    implicit case object TIntIsNumeric        extends AbstractIsNumeric[Int]
    implicit case object TLongIsNumeric       extends AbstractIsNumeric[Long]
    implicit case object TFloatIsNumeric      extends AbstractIsNumeric[Float]
    implicit case object TDoubleIsNumeric     extends AbstractIsNumeric[Double]
    implicit case object TBigDecimalIsNumeric extends AbstractIsNumeric[BigDecimal]
  }

  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]
    type Append[That <: ColumnSet] <: ColumnSet

    def ++[That <: ColumnSet](that: That): Append[That]

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

      override def ++[That <: ColumnSet](that: That): Append[That] = that

      override def columnsUntyped: List[Column.Untyped] = Nil

      override protected def mkColumns[T](name: TableName): ColumnsRepr[T] = ()
    }

    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>
      type ColumnsRepr[T]            = (Expr[Features.Source, T, A], tail.ColumnsRepr[T])
      type Append[That <: ColumnSet] = Cons[A, tail.Append[That]]

      override def ++[That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)

      override def columnsUntyped: List[Column.Untyped] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Source.Aux_[ColumnsRepr, A :*: B] =
        new Table.Source {
          type Repr[C] = ColumnsRepr[C]
          type Cols    = A :*: B
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
    def uuid(name: String): Singleton[UUID]                     = singleton[UUID](name)
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

    val columnsUntyped: List[Column.Untyped]
  }

  object Table {

    class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {

      def on[F](expr: Expr[F, A with B, Boolean]): Table.Aux[A with B] =
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    sealed trait Source extends Table {
      type Repr[_]
      type Cols
      val name: TableName
      val columnSchema: ColumnSchema[Cols]
      val columns: Repr[TableType]
    }
    object Source {
      type Aux_[F[_], B] = Table.Source {
        type Repr[X] = F[X]
        type Cols    = B
      }
      type Aux[F[_], A, B] = Table.Source {
        type Repr[X]   = F[X]
        type TableType = A
        type Cols      = B
      }
    }

    sealed case class Joined[F, A, B](
      joinType: JoinType,
      left: Table.Aux[A],
      right: Table.Aux[B],
      on: Expr[F, A with B, Boolean]
    ) extends Table {
      type TableType = left.TableType with right.TableType
      val columnsUntyped: List[Column.Untyped] = left.columnsUntyped ++ right.columnsUntyped
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

  def deleteFrom[F[_], A, B](table: Table.Source.Aux[F, A, B]): DeleteBuilder[F, A, B] = DeleteBuilder(table)

  def update[A](table: Table.Aux[A]): Update[A] = Update(table, Nil, true)

  sealed case class SelectBuilder[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) {

    def from[A1 <: A](table: Table.Aux[A1]): Read.Select[F, A1, B] =
      Read.Select(selection, table, true, Nil)
  }

  sealed case class DeleteBuilder[F[_], A, B](table: Table.Aux[A]) {
    def where[F1](expr: Expr[F1, A, Boolean]): Delete[F1, A] = Delete(table, expr)
  }

  sealed case class Delete[F, A](
    table: Table.Aux[A],
    whereExpr: Expr[F, A, Boolean]
  )

  // UPDATE table
  // SET foo = bar
  // WHERE baz > buzz
  sealed case class Update[A](table: Table.Aux[A], set: List[Set[_, A]], whereExpr: Expr[_, A, Boolean]) {

    def set[F: Features.IsSource, Value: TypeTag](lhs: Expr[F, A, Value], rhs: Expr[_, A, Value]): Update[A] =
      copy(set = set :+ Set(lhs, rhs))

    def where(whereExpr2: Expr[_, A, Boolean]): Update[A] =
      copy(whereExpr = whereExpr && whereExpr2)
  }

  sealed trait Set[F, -A] {
    type Value

    def lhs: Expr[F, A, Value]
    def rhs: Expr[_, A, Value]

    def typeTag: TypeTag[Value]
  }

  object Set {
    type Aux[F, -A, Value0] = Set[F, A] { type Value = Value0 }

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

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[F, +A] { self =>
    def union[F1, A1 >: A](that: Read[F1, A1]): Read[F :||: F1, A1] = Read.Union(self, that, true)

    def unionAll[F1, A1 >: A](that: Read[F1, A1]): Read[F :||: F1, A1] = Read.Union(self, that, false)
  }

  object Read {

    sealed case class Select[F, A, B <: SelectionSet[A]](
      selection: Selection[F, A, B],
      table: Table.Aux[A],
      whereExpr: Expr[_, A, Boolean],
      groupBy: List[Expr[_, A, Any]],
      orderBy: List[Ordering[Expr[_, A, Any]]] = Nil,
      offset: Option[Long] = None,
      limit: Option[Long] = None
    ) extends Read[F, B] { self =>

      def where(whereExpr2: Expr[_, A, Boolean]): Select[F, A, B] =
        copy(whereExpr = self.whereExpr && whereExpr2)

      def limit(n: Long): Select[F, A, B] = copy(limit = Some(n))

      def offset(n: Long): Select[F, A, B] = copy(offset = Some(n))

      def orderBy(o: Ordering[Expr[_, A, Any]], os: Ordering[Expr[_, A, Any]]*): Select[F, A, B] =
        copy(orderBy = self.orderBy ++ (o :: os.toList))

      def groupBy(key: Expr[_, A, Any], keys: Expr[_, A, Any]*)(
        implicit ev: Features.IsAggregated[F]
      ): Select[F, A, B] = {
        val _ = ev
        copy(groupBy = groupBy ++ (key :: keys.toList))
      }
    }

    sealed case class Union[F1, F2, B](left: Read[F1, B], right: Read[F2, B], distinct: Boolean)
        extends Read[F1 :||: F2, B]

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[Features.Literal, B]

    def lit[B: TypeTag](values: B*): Read[Features.Literal, B] = Literal(values.toSeq)
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

    def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T]
  }

  object SelectionSet {
    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {
      override type SelectionsRepr[Source1, T] = Unit

      override type Append[Source1, That <: SelectionSet[Source1]] = That

      override def ++[Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      def selections[Source1 <: Any, T]: SelectionsRepr[Source1, T] = ()
    }

    sealed case class Cons[-Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B)
        extends SelectionSet[Source] { self =>
      override type SelectionsRepr[Source1, T] = (ColumnSelection[Source1, A], tail.SelectionsRepr[Source1, T])

      override type Append[Source1, That <: SelectionSet[Source1]] =
        Cons[Source1, A, tail.Append[Source1, That]]

      override def ++[Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] =
        Cons[Source1, A, tail.Append[Source1, That]](head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      def selections[Source1 <: Source, T]: SelectionsRepr[Source1, T] = (head, tail.selections[Source1, T])
    }
  }

  sealed trait UnaryOp[A]

  object UnaryOp {
    sealed case class Negate[A: IsNumeric]() extends UnaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
    }

    sealed case class NotBit[A: IsIntegral]() extends UnaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
    }

    case object NotBool extends UnaryOp[Boolean]
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

    case object AndBool extends BinaryOp[Boolean]
    case object OrBool  extends BinaryOp[Boolean]

    sealed case class AndBit[A: IsIntegral]() extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
    }
    sealed case class OrBit[A: IsIntegral]() extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
    }
  }

  sealed trait PropertyOp

  object PropertyOp {
    case object IsNull    extends PropertyOp
    case object IsNotNull extends PropertyOp
    case object IsTrue    extends PropertyOp
    case object IsNotTrue extends PropertyOp
  }

  sealed trait RelationalOp

  object RelationalOp {
    case object Equals           extends RelationalOp
    case object GreaterThan      extends RelationalOp
    case object GreaterThanEqual extends RelationalOp
    case object LessThan         extends RelationalOp
    case object LessThanEqual    extends RelationalOp
    case object NotEqual         extends RelationalOp
  }

  type :||:[A, B] = Features.Union[A, B]

  /**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
  sealed trait Expr[F, -A, +B] { self =>

    def +[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Add[B1]())

    def -[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Sub[B1]())

    def *[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Mul[B1]())

    def /[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsNumeric[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.Div[B1]())

    def &&[F2, A1 <: A, B1 >: B](
      that: Expr[F2, A1, Boolean]
    )(implicit ev: B <:< Boolean): Expr[F :||: F2, A1, Boolean] =
      Expr.Binary(self.widen[Boolean], that, BinaryOp.AndBool)

    def ||[F2, A1 <: A, B1 >: B](
      that: Expr[F2, A1, Boolean]
    )(implicit ev: B <:< Boolean): Expr[F :||: F2, A1, Boolean] =
      Expr.Binary(self.widen[Boolean], that, BinaryOp.OrBool)

    def ===[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.Equals)

    def <>[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.NotEqual)

    def >[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.GreaterThan)

    def <[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.LessThan)

    def >=[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.GreaterThanEqual)

    def <=[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1]): Expr[F :||: F2, A1, Boolean] =
      Expr.Relational(self, that, RelationalOp.LessThanEqual)

    def &[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsIntegral[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.AndBit[B1])

    def |[F2, A1 <: A, B1 >: B](that: Expr[F2, A1, B1])(implicit ev: IsIntegral[B1]): Expr[F :||: F2, A1, B1] =
      Expr.Binary(self, that, BinaryOp.OrBit[B1])

    def unary_~[B1 >: B](implicit ev: IsIntegral[B1]): Expr.Unary[F, A, B1] =
      Expr.Unary(self, UnaryOp.NotBit[B1])

    def unary_-[B1 >: B](implicit ev: IsNumeric[B1]): Expr.Unary[F, A, B1] =
      Expr.Unary(self, UnaryOp.Negate[B1])

    def not[A1 <: A](implicit ev: B <:< Boolean): Expr.Unary[F, A1, Boolean] =
      Expr.Unary(self.widen[Boolean], UnaryOp.NotBool)

    def isNull[A1 <: A]: Expr[F, A1, Boolean] =
      Expr.Property(self, PropertyOp.IsNull)

    def isNotNull[A1 <: A]: Expr[F, A1, Boolean] =
      Expr.Property(self, PropertyOp.IsNotNull)

    def isTrue[A1 <: A](implicit ev: B <:< Boolean): Expr[F, A1, Boolean] =
      Expr.Property(self, PropertyOp.IsTrue)

    def isNotTrue[A1 <: A](implicit ev: B <:< Boolean): Expr[F, A1, Boolean] =
      Expr.Property(self, PropertyOp.IsNotNull)

    def as[B1 >: B](name: String): Selection[F, A, SelectionSet.Cons[A, B1, SelectionSet.Empty]] =
      Selection.computedAs(self, name)

    def ascending: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

    def asc: Ordering[Expr[F, A, B]] = Ordering.Asc(self)

    def descending: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

    def desc: Ordering[Expr[F, A, B]] = Ordering.Desc(self)

    def in[F1, B1 >: B](set: Read[F1, B1]): Expr[F, A, Boolean] = Expr.In(self, set)

    def widen[C](implicit ev: B <:< C): Expr[F, A, C] = {
      val _ = ev
      self.asInstanceOf[Expr[F, A, C]]
    }
  }

  object Features {
    type SingleColumnSelect[_]
    type Aggregated[_]
    type Union[_, _]
    type Source
    type Literal

    sealed trait IsAggregated[A]

    object IsAggregated {
      def apply[A](implicit is: IsAggregated[A]): IsAggregated[A] = is

      implicit def AggregatedIsAggregated[A]: IsAggregated[Aggregated[A]] = ???

      implicit def UnionIsAggregated[A: IsAggregated, B: IsAggregated]: IsAggregated[Union[A, B]] = ???
    }

    @implicitNotFound("You can only use this function on a column in the source table")
    sealed trait IsSource[A]

    object IsSource {
      implicit case object SourceIsSource extends IsSource[Source]
    }
  }

  object Expr {
    implicit def literal[A: TypeTag](a: A): Expr[Features.Literal, Any, A] = Expr.Literal(a)

    def exprName[F, A, B](expr: Expr[F, A, B]): Option[String] =
      expr match {
        case Expr.Source(_, c) => Some(c.name)
        case _                 => None
      }

    implicit def expToSelection[F, A, B](
      expr: Expr[F, A, B]
    ): Selection[F, A, SelectionSet.Cons[A, B, SelectionSet.Empty]] =
      Selection.computedOption(expr, exprName(expr))

    sealed case class Source[A, B] private[Sql] (tableName: TableName, column: Column[B])
        extends Expr[Features.Source, A, B]

    sealed case class Unary[F, -A, B](base: Expr[F, A, B], op: UnaryOp[B]) extends Expr[F, A, B]

    sealed case class Property[F, -A, +B](base: Expr[F, A, B], op: PropertyOp) extends Expr[F, A, Boolean]

    sealed case class Binary[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: BinaryOp[B])
        extends Expr[Features.Union[F1, F2], A, B]

    sealed case class Relational[F1, F2, A, B](left: Expr[F1, A, B], right: Expr[F2, A, B], op: RelationalOp)
        extends Expr[Features.Union[F1, F2], A, Boolean]

    sealed case class In[F, F1, A, B](value: Expr[F, A, B], set: Read[F1, B]) extends Expr[F, A, Boolean]

    sealed case class Literal[B: TypeTag](value: B) extends Expr[Features.Literal, Any, B]

    sealed case class AggregationCall[F, A, B, Z](param: Expr[F, A, B], aggregation: AggregationDef[B, Z])
        extends Expr[Features.Aggregated[F], A, Z]

    sealed case class FunctionCall1[F, A, B, Z](param: Expr[F, A, B], function: FunctionDef[B, Z]) extends Expr[F, A, Z]

    sealed case class FunctionCall2[F1, F2, A, B, C, Z](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      function: FunctionDef[(B, C), Z]
    ) extends Expr[Features.Union[F1, F2], A, Z]

    sealed case class FunctionCall3[F1, F2, F3, A, B, C, D, Z](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      param3: Expr[F3, A, D],
      function: FunctionDef[(B, C, D), Z]
    ) extends Expr[Features.Union[F1, Features.Union[F2, F3]], A, Z]

    sealed case class FunctionCall4[F1, F2, F3, F4, A, B, C, D, E, Z](
      param1: Expr[F1, A, B],
      param2: Expr[F2, A, C],
      param3: Expr[F3, A, D],
      param4: Expr[F4, A, E],
      function: FunctionDef[(B, C, D, E), Z]
    ) extends Expr[Features.Union[F1, Features.Union[F2, Features.Union[F3, F4]]], A, Z]
  }

  sealed case class AggregationDef[-A, +B](name: FunctionName) { self =>

    def apply[F, Source](expr: Expr[F, Source, A]): Expr[Features.Aggregated[F], Source, B] =
      Expr.AggregationCall(expr, self)
  }

  object AggregationDef {
    val Count: AggregationDef[Any, Long] = AggregationDef(FunctionName("count"))
    val Sum                              = AggregationDef[Double, Double](FunctionName("sum"))
    val Arbitrary                        = AggregationDef[Any, Any](FunctionName("arbitrary"))
    val Avg                              = AggregationDef[Double, Double](FunctionName("avg"))
    val Min                              = AggregationDef[Any, Any](FunctionName("min"))
    val Max                              = AggregationDef[Any, Any](FunctionName("max"))
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName) { self =>
    def apply[F, Source](param1: Expr[F, Source, A]): Expr[F, Source, B] = Expr.FunctionCall1(param1, self)

    def apply[F1, F2, Source, P1, P2](param1: Expr[F1, Source, P1], param2: Expr[F2, Source, P2])(
      implicit ev: (P1, P2) <:< A
    ): Expr[F1 :||: F2, Source, B] =
      Expr.FunctionCall2(param1, param2, self.narrow[(P1, P2)])

    def apply[F1, F2, F3, Source, P1, P2, P3](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3]
    )(implicit ev: (P1, P2, P3) <:< A): Expr[F1 :||: F2 :||: F3, Source, B] =
      Expr.FunctionCall3(param1, param2, param3, self.narrow[(P1, P2, P3)])

    def apply[F1, F2, F3, F4, Source, P1, P2, P3, P4](
      param1: Expr[F1, Source, P1],
      param2: Expr[F2, Source, P2],
      param3: Expr[F3, Source, P3],
      param4: Expr[F4, Source, P4]
    )(implicit ev: (P1, P2, P3, P4) <:< A): Expr[F1 :||: F2 :||: F3 :||: F4, Source, B] =
      Expr.FunctionCall4(param1, param2, param3, param4, self.narrow[(P1, P2, P3, P4)])

    def narrow[C](implicit ev: C <:< A): FunctionDef[C, B] = {
      val _ = ev
      self.asInstanceOf[FunctionDef[C, B]]
    }
  }

  object FunctionDef {
    //math functions
    val Abs   = FunctionDef[Double, Double](FunctionName("abs"))
    val Acos  = FunctionDef[Double, Double](FunctionName("acos"))
    val Asin  = FunctionDef[Double, Double](FunctionName("asin"))
    val Atan  = FunctionDef[Double, Double](FunctionName("atan"))
    val Ceil  = FunctionDef[Double, Double](FunctionName("ceil"))
    val Cos   = FunctionDef[Double, Double](FunctionName("cos"))
    val Exp   = FunctionDef[Double, Double](FunctionName("exp"))
    val Floor = FunctionDef[Double, Double](FunctionName("floor"))
    //val Log = FunctionDef[Double, Double](FunctionName("log")) //not part of SQL 2011 spec
    val Ln          = FunctionDef[Double, Double](FunctionName("ln"))
    val Mod         = FunctionDef[(Double, Double), Double](FunctionName("mod"))
    val Power       = FunctionDef[(Double, Double), Double](FunctionName("power"))
    val Round       = FunctionDef[(Double, Int), Double](FunctionName("round"))
    val Sign        = FunctionDef[Double, Double](FunctionName("sign"))
    val Sin         = FunctionDef[Double, Double](FunctionName("sin"))
    val Sqrt        = FunctionDef[Double, Double](FunctionName("sqrt"))
    val Tan         = FunctionDef[Double, Double](FunctionName("tan"))
    val WidthBucket = FunctionDef[(Double, Double, Double, Int), Int](FunctionName("width bucket"))

    //string functions
    val Ascii       = FunctionDef[String, Int](FunctionName("ascii"))
    val CharLength  = FunctionDef[String, Int](FunctionName("character length"))
    val Concat      = FunctionDef[(String, String), String](FunctionName("concat"))
    val Lower       = FunctionDef[String, String](FunctionName("lower"))
    val Ltrim       = FunctionDef[String, String](FunctionName("ltrim"))
    val OctetLength = FunctionDef[String, Int](FunctionName("octet length"))
    val Overlay     = FunctionDef[(String, String, Int, Option[Int]), String](FunctionName("overlay"))
    val Position    = FunctionDef[(String, String), Int](FunctionName("position"))
    val Replace     = FunctionDef[(String, String), String](FunctionName("replace"))
    val Rtrim       = FunctionDef[String, String](FunctionName("rtrim"))
    val Substring   = FunctionDef[(String, Int, Option[Int]), String](FunctionName("substring"))
    //TODO substring regex
    val Trim  = FunctionDef[String, String](FunctionName("trim"))
    val Upper = FunctionDef[String, String](FunctionName("upper"))

    // date functions
    val CurrentTimestamp = FunctionDef[Nothing, Instant](FunctionName("current_timestamp"))
  }

  object Example1 {
    import ColumnSet._

    val columnSet = int("age") ++ string("name")

    val table = columnSet.table("person")

    val table2 = columnSet.table("person2")

    val age :*: name :*: _ = table.columns

    val age2 :*: name2 :*: _ = table2.columns

    import FunctionDef._
    import AggregationDef._

    val queried =
      (select {
        ((age + 2) as "age") ++ (name as "name") ++ (Abs(3.0) as "dummy")
      } from table)
        .limit(200)
        .offset(1000)
        .orderBy(age.descending)

    val joined =
      select {
        (age as "age") ++ (age2 as "age2")
      } from (table join table2).on(name === name2)

    val aggregated =
      (select {
        (Arbitrary(age) as "age") ++ (Count(1) as "count")
      } from table) groupBy age

    val deleted = deleteFrom(table).where(age === 3)

    val updated =
      update(table)
        .set(age, age + 2)
        .set(name, "foo")
        .where(age > 100)
  }
}
