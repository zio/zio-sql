package zio.sql

import scala.language.implicitConversions

import java.time._

trait Sql {
  type ColumnName = String
  type TableName = String

  sealed trait TypeTag[A]
  object TypeTag {
    implicit case object TBigDecimal extends TypeTag[BigDecimal]
    implicit case object TBoolean extends TypeTag[Boolean]
    implicit case object TByteArray extends TypeTag[Array[Byte]]
    implicit case object TChar extends TypeTag[Char]
    implicit case object TDouble extends TypeTag[Double]
    implicit case object TFloat extends TypeTag[Float]
    implicit case object TInstant extends TypeTag[Instant]
    implicit case object TInt extends TypeTag[Int]
    implicit case object TLocalDate extends TypeTag[LocalDate]
    implicit case object TLocalDateTime extends TypeTag[LocalDateTime]
    implicit case object TLocalTime extends TypeTag[LocalTime]
    implicit case object TLong extends TypeTag[Long]
    implicit case object TOffsetDateTime extends TypeTag[OffsetDateTime]
    implicit case object TOffsetTime extends TypeTag[OffsetTime]
    implicit case object TShort extends TypeTag[Short]
    implicit case object TString extends TypeTag[String]
    implicit case object TZonedDateTime extends TypeTag[ZonedDateTime]
  }

  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]

    def :*: [A](head: Column[A]): ColumnSet

    def columnsUntyped: List[Column[_]]

    protected def mkColumns[T](name: TableName): ColumnsRepr[T]
  }
  object ColumnSet {
    type Empty = Empty.type
    type :*: [A, B <: ColumnSet] = Cons[A, B]

    case object Empty extends ColumnSet {
      type ColumnsRepr[_] = Unit

      override def :*: [A](head: Column[A]): A :*: Empty = Cons(head, Empty)

      override def columnsUntyped: List[Column[_]] = Nil

      override protected def mkColumns[T](name: TableName): ColumnsRepr[T] = ()
    }
    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>     
      type ColumnsRepr[T] = (Expr[T, A], tail.ColumnsRepr[T])

      override def :*: [C](head: Column[C]): C :*: A :*: B = Cons(head, self)

      override def columnsUntyped: List[Column[_]] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Source[ColumnsRepr, A :*: B] = 
        new Table.Source[ColumnsRepr, A :*: B] {
          val name: TableName = name0
          val columnSchema: ColumnSchema[A :*: B] = ColumnSchema(self)
          val columns: ColumnsRepr[TableType] = mkColumns[TableType](name0)
          val columnsUntyped: List[Column[_]] = self.columnsUntyped
        }

      override protected def mkColumns[T](name: TableName): ColumnsRepr[T] = 
        (Expr.Source(name, head), tail.mkColumns(name))
    }
    import Column._ 

    val columnSet = int("age") :*: string("name") :*: Empty

    val table = columnSet.table("person")

    val table2 = columnSet.table("person2")

    val age :*: name :*: _ = table.columns

    val age2 :*: name2 :*: _ = table2.columns

    val joined = (table join table2) on {
      age === age2
    }
  }

  object :*: {
    def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
  }

  sealed case class FunctionName(name: String)

  sealed case class Column[A: TypeTag](name: String) {
    def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
  }
  object Column {                                                                           // | SQL                          | PostgreSQL                           | T-SQL
    def bigDecimal(name: String): Column[BigDecimal]         = Column[BigDecimal](name)     // | decimal/dec/numeric          | decimal/numeric                      | decimal/numeric
    def boolean(name: String): Column[Boolean]               = Column[Boolean](name)        // | boolean                      | boolean                              | bit
    def byteArray(name: String): Column[Array[Byte]]         = Column[Array[Byte]](name)    // | blob                         | bytea                                | varbinary(max)
    def char(name: String): Column[Char]                     = Column[Char](name)           // | char(n)/character(n)         | char/character/char(1)/character(1)  | nchar(1)
    def double(name: String): Column[Double]                 = Column[Double](name)         // | double precision/float(n)    | double precision/float(25-53)/float8 | double precision/float(53)
    def float(name: String): Column[Float]                   = Column[Float](name)          // | real/float(n)                | real/float(1-24)/float4              | real/float(24)
    def instant(name: String): Column[Instant]               = Column[Instant](name)        // | timestamp (WITHOUT TIMEZONE) | [*] timestamp (WITHOUT TIMEZONE)     | ???
    def int(name: String): Column[Int]                       = Column[Int](name)            // | int/integer                  | int/integer/int4                     | int
    def localDate(name: String): Column[LocalDate]           = Column[LocalDate](name)      // | date                         | date                                 | date
    def localDateTime(name: String): Column[LocalDateTime]   = Column[LocalDateTime](name)  // | timestamp (WITHOUT TIMEZONE) | timestamp (WITHOUT TIMEZONE)         | datetime/datetime2
    def localTime(name: String): Column[LocalTime]           = Column[LocalTime](name)      // | time (WITHOUT TIMEZONE)      | time (WITHOUT TIMEZONE)              | time
    def long(name: String): Column[Long]                     = Column[Long](name)           // | bigint                       | bigint/int8                          | bigint
    def offsetDateTime(name: String): Column[OffsetDateTime] = Column[OffsetDateTime](name) // | timestamp WITH TIMEZONE      | timestamp WITH TIMEZONE              | datetimeoffset
    def offsetTime(name: String): Column[OffsetTime]         = Column[OffsetTime](name)     // | time WITH TIMEZONE           | time WITH TIMEZONE                   | ???
    def short(name: String): Column[Short]                   = Column[Short](name)          // | smallint                     | smallint/int2                        | smallint
    def string(name: String): Column[String]                 = Column[String](name)         // | varchar(n)                   | [**] varchar                         | [**] nvarchar(max)
    def zonedDateTime(name: String): Column[ZonedDateTime]   = Column[ZonedDateTime](name)  // | timestamp WITH TIMEZONE      | timestamp WITH TIMEZONE              | datetimeoffset

    // [*] Java Instant has nanosecond resolution while PostgreSQL's date/time data types are in microseconds
    // [**] 'n' in varchar(n) mean different things - in PostgreSQL it's number of chars, but in T-SQL it's number of bytes. That's why for T-SQL it's nvarchar.
    //
    //TODO research: how to represent UUID in different dbs
    //TODO research: how to represent byte in different dbs
    //TODO research: SQL defines 'interval' type. See if databases support it and if it's useful for some Scala types (eg. FiniteDuration)
    
    //ANSI SQL BNF 2011 ref: http://jakewheat.github.io/sql-overview/sql-2011-foundation-grammar.html#predefined-type
    //PostgreSQL Data types https://www.postgresql.org/docs/12/datatype.html
    //T-SQL Data types https://docs.microsoft.com/en-us/sql/t-sql/data-types/data-types-transact-sql?view=sql-server-ver15
  }

  sealed trait JoinType
  object JoinType {
    case object Inner       extends JoinType
    case object LeftOuter   extends JoinType
    case object RightOuter  extends JoinType
    case object FullOuter   extends JoinType
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
      def on(expr: Expr[A with B, Boolean]): Table.Aux[A with B] = 
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    sealed trait Source[F[_], A] extends Table {
      val name: TableName
      val columnSchema: ColumnSchema[A]
      val columns: F[TableType]
      val columnsUntyped: List[Column[_]]
    } 

    sealed case class Joined[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B], on: Expr[A with B, Boolean]) extends Table {
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
   */
  def select[A, B <: SelectionSet[A]](selection: Selection[A, B]): SelectBuilder[A, B] = 
    SelectBuilder(selection)

  sealed case class SelectBuilder[A, B <: SelectionSet[A]](selection: Selection[A, B]) {
    def from(table: Table.Aux[A]): Read.Select[A, B] = 
      Read.Select(selection, table, Expr.Literal(true))
  }
  implicit def literal[A: TypeTag](a: A): Expr[Any, A] = Expr.Literal(a)

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+A]
  object Read {
    sealed case class Select[A, B <: SelectionSet[A]](
      selection: Selection[A, B], table: Table.Aux[A], 
      whereExpr: Expr[A, Boolean], 
      orderBy: List[Ordering[Expr[A, Any]]] = Nil, 
      offset: Option[Long] = None, limit: Option[Long] = None) extends Read[B] { self =>
      def where(whereExpr2: Expr[A, Boolean]): Select[A, B] = 
        copy(whereExpr = Expr.Binary(self.whereExpr, whereExpr2, BinaryOp.AndBool))

      def limit(n: Long): Select[A, B] = copy(limit = Some(n))

      def offset(n: Long): Select[A, B] = copy(offset = Some(n))

      def orderBy(o: Ordering[Expr[A, Any]], os: Ordering[Expr[A, Any]]*): Select[A, B] = 
        copy(orderBy = self.orderBy ++ (o :: os.toList))
    }
    
    sealed case class Union[B](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B]

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[B]
  }

  sealed trait Ordering[+A]
  object Ordering {
    sealed case class Asc[A](value: A) extends Ordering[A]
    sealed case class Desc[A](value: A) extends Ordering[A]

    implicit def exprToOrdering[A, B](expr: Expr[A, B]): Ordering[Expr[A, B]] = 
      Asc(expr)
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[-A, +B <: SelectionSet[A]](value: B) { self =>
    type SelectionType 

    def ++ [A1 <: A, C <: SelectionSet[A1]](that: Selection[A1, C]): Selection[A1, self.value.Append[A1, C]] = 
      Selection(self.value ++ that.value)

    def columns: value.SelectionsRepr[SelectionType] = value.selections[SelectionType]
  }
  object Selection {
    import SelectionSet.{ Empty, Cons }
    import ColumnSelection._

    val empty: Selection[Any, Empty] = Selection(Empty)

    def constantOption[A: TypeTag](value: A, option: Option[ColumnName]): Selection[Any, Cons[Any, A, Empty]] = 
      Selection(Cons(Constant(value, option), Empty))

    def constant[A: TypeTag](value: A): Selection[Any, Cons[Any, A, Empty]] = constantOption(value, None)

    def constantAs[A: TypeTag](value: A, name: ColumnName): Selection[Any, Cons[Any, A, Empty]] = constantOption(value, Some(name))

    def computedOption[A, B](expr: Expr[A, B], name: Option[ColumnName]): Selection[A, Cons[A, B, Empty]] =
      Selection(Cons(Computed(expr, name), Empty))

    def computed[A, B](expr: Expr[A, B]): Selection[A, Cons[A, B, Empty]] =
      computedOption(expr, None)

    def computedAs[A, B](expr: Expr[A, B], name: ColumnName): Selection[A, Cons[A, B, Empty]] =
      computedOption(expr, Some(name))

    val selection = 
      computed(Expr.FunctionCall(Expr.Literal("test"), FunctionDef.CharLength)) ++ 
      constant(1) ++ empty ++ constant("foo") ++ constant(true) ++ empty

    val int :*: str :*: bool :*: _ = selection.columns
  }
  
  sealed trait ColumnSelection[-A, +B]
  object ColumnSelection {
    sealed case class Constant[A: TypeTag](value: A, name: Option[ColumnName]) extends ColumnSelection[Any, A]
    sealed case class Computed[A, B](expr: Expr[A, B], name: Option[ColumnName]) extends ColumnSelection[A, B]
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[T]
    
    type Append[Source1 <: Source, That <: SelectionSet[Source1]] <: SelectionSet[Source1]

    def :*: [Source1 <: Source, A](head: ColumnSelection[Source1, A]): SelectionSet[Source1]

    def ++ [Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    def selections[T]: SelectionsRepr[T]
  }
  object SelectionSet {
    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {
      override type SelectionsRepr[T] = Unit

      override type Append[Source1 <: Any, That <: SelectionSet[Source1]] = That

      override def :*: [Source1 <: Any, A](head: ColumnSelection[Source1, A]) = Cons(head, Empty)

      override def ++ [Source1 <: Any, That <: SelectionSet[Source1]](that: That): Append[Source1, That] = 
        that

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      def selections[T]: SelectionsRepr[T] = ()
    }
    sealed case class Cons[Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B) extends SelectionSet[Source] { self =>     
      override type SelectionsRepr[T] = (ColumnSelection[Source, A], tail.SelectionsRepr[T])
      override type Append[Source1 <: Source, That <: SelectionSet[Source1]] = 
        Cons[Source1, A, tail.Append[Source1, That]]

      override def :*: [Source1 <: Source, C](head: ColumnSelection[Source1, C]) = Cons(head, self)

      override def ++ [Source1 <: Source, That <: SelectionSet[Source1]](that: That): Append[Source1, That] = 
        Cons(head, tail ++ that)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      def selections[T]: SelectionsRepr[T] = (head, tail.selections[T])
    }
  }

  sealed trait BinaryOp[A]
  object BinaryOp {
    case object AddInt extends BinaryOp[Int]
    case object SubInt extends BinaryOp[Int]
    case object MultInt extends BinaryOp[Int]
    case object DivInt extends BinaryOp[Int]
    case object ModInt extends BinaryOp[Int]
    case object AddLong extends BinaryOp[Long]
    case object SubLong extends BinaryOp[Long]
    case object MultLong extends BinaryOp[Long]
    case object DivLong extends BinaryOp[Long]
    case object ModLong extends BinaryOp[Long]
    case object AndBool extends BinaryOp[Boolean]
    case object OrBool extends BinaryOp[Boolean]
    case object StringConcat extends BinaryOp[String]
  }
  sealed trait RelationalOp
  object RelationalOp {
    case object Equals extends RelationalOp
    case object LessThan extends RelationalOp
    case object GreaterThan extends RelationalOp
    case object LessThanEqual extends RelationalOp
    case object GreaterThanEqual extends RelationalOp
  }

  /**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
  sealed trait Expr[-A, +B] { self => 
    def === [A1 <: A, B1 >: B](that: Expr[A1, B1]): Expr[A1, Boolean] = 
      Expr.Relational(self, that, RelationalOp.Equals)

    def > [A1 <: A, B1 >: B](that: Expr[A1, B1]): Expr[A1, Boolean] = 
      Expr.Relational(self, that, RelationalOp.GreaterThan)

    def < [A1 <: A, B1 >: B](that: Expr[A1, B1]): Expr[A1, Boolean] = 
      Expr.Relational(self, that, RelationalOp.LessThan)

    def >= [A1 <: A, B1 >: B](that: Expr[A1, B1]): Expr[A1, Boolean] = 
      Expr.Relational(self, that, RelationalOp.GreaterThanEqual)

    def <= [A1 <: A, B1 >: B](that: Expr[A1, B1]): Expr[A1, Boolean] = 
      Expr.Relational(self, that, RelationalOp.LessThanEqual)

    def ascending: Ordering[Expr[A, B]] = Ordering.Asc(self)

    def asc: Ordering[Expr[A, B]] = Ordering.Asc(self)

    def descending: Ordering[Expr[A, B]] = Ordering.Desc(self)

    def desc: Ordering[Expr[A, B]] = Ordering.Desc(self)
  }
  object Expr {
    sealed case class Source[A, B] private [Sql] (tableName: TableName, column: Column[B]) extends Expr[A, B]
    sealed case class Binary[A, B](left: Expr[A, B], right: Expr[A, B], op: BinaryOp[B]) extends Expr[A, B]
    sealed case class Relational[A, B](left: Expr[A, B], right: Expr[A, B], op: RelationalOp) extends Expr[A, Boolean]
    sealed case class In[A, B](value: Expr[A, B], set: Read[B]) extends Expr[A, Boolean]
    sealed case class Literal[B: TypeTag](value: B) extends Expr[Any, B]
    sealed case class FunctionCall[A, B, C](value: Expr[A, B], function: FunctionDef[B, C])
      extends Expr[A, C]
    // a IN ("foo", "bar")
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName)
  object FunctionDef {

    case class ModOperands(x: Double, y: Double)
    case class PowerOperands(base: Double, exp: Double)
    case class WidthBucketOperands(operand: Double, b1: Double, b2: Double, count: Int)
    case class OverlayOperands(source: String, _with: String, from: Int, _for: Option[Int] = None)
    case class PositionOperands(substring: String, source: String)
    case class SubstringOperands(source: String, from: Int, to: Option[Int])
    case class TrimOperands(source: String, spec: Option[TrimSpec] = None)
    trait TrimSpec
    case class Leading(chars: Option[String] = None) extends TrimSpec
    case class Trailing(chars: Option[String] = None) extends TrimSpec
    case class Both(chars: Option[String] = None) extends TrimSpec

    //match functions
    val Abs = FunctionDef[Double, Double](FunctionName("abs"))
    val Ceil = FunctionDef[Double, Double](FunctionName("ceil"))
    val Exp = FunctionDef[Double, Double](FunctionName("exp"))
    val Floor = FunctionDef[Double, Double](FunctionName("floor"))
    //val Log = FunctionDef[Double, Double](FunctionName("log")) //not part of SQL 2011 spec
    val Ln = FunctionDef[Double, Double](FunctionName("ln"))
    val Mod = FunctionDef[ModOperands, Double](FunctionName("mod"))
    val Power = FunctionDef[PowerOperands, Double](FunctionName("power"))
    val Sqrt = FunctionDef[Double, Double](FunctionName("sqrt"))
    val WidthBucket = FunctionDef[WidthBucketOperands, Int](FunctionName("width bucket"))

    //string functions
    val CharLength = FunctionDef[String, Int](FunctionName("character length"))
    val Lower = FunctionDef[String, String](FunctionName("lower"))
    val OctetLength = FunctionDef[String, Int](FunctionName("octet length"))
    val Overlay = FunctionDef[OverlayOperands, String](FunctionName("overlay"))
    val Position = FunctionDef[PositionOperands, Int](FunctionName("position"))
    val Substring = FunctionDef[SubstringOperands, String](FunctionName("substring"))
    //TODO substring regex
    val Trim = FunctionDef[TrimOperands, String](FunctionName("trim"))
    val Upper = FunctionDef[String, String](FunctionName("upper"))
  }
}

/*
  
 def query(limit: Int) = {
    val selection = age * 2 ~ name ~ username

    val newAge :*: newName :*: newUsername = selection

    select(selection)
      .from(person)
      .where(age === 42)
      .limit(limit)
      .orderBy(newAge.ascending, newName.descending)
 }

val query = 
  Param.int { limit =>
    select(age * 2 ~ name ~ username)
      .from(person)
      .where(age === lit(42))
      .limit(limit)
  }

  query(200)
*/

/*
sealed trait Predicate[-A]
  // sealed trait ExistencePredicate extends Predicate[Any]
  // object Predicate {
  //   sealed case class Equal[A](right: A) extends Predicate[A]
  //   sealed case class NoEqual[A](right: A) extends Predicate[A]
  //   sealed case class GreaterThan[A](right: A) extends Predicate[A]
  //   sealed case class LessThan[A](right: A) extends Predicate[A]
  //   sealed case class GreaterThanOrEqual[A](right: A) extends Predicate[A]
  //   sealed case class LessThanOrEqual[A](right: A) extends Predicate[A]
  //   case object IsNull extends Predicate[Any]
  //   case object IsNotNull extends Predicate[Any]
  //   case object IsTrue extends Predicate[Boolean]
  //   case object IsNotTrue extends Predicate[Boolean]
  //   case object IsFalse extends Predicate[Boolean]
  //   case object IsNotFalse extends Predicate[Boolean]
  //   sealed case class Between[A](lower: A, upper: A) extends Predicate[A]
  //   sealed case class Like(right: String) extends Predicate[String]
    
  //   sealed case class In[A](right: Set[A]) extends Predicate[A] //could be applied to a subquery result
  //   sealed case class NotIn[A](right: Set[A]) extends Predicate[A] //could be applied to a subquery result
    
  //   sealed case class Exists(read: Read[_]) extends ExistencePredicate
  //   sealed case class NotExists(read: Read[_]) extends ExistencePredicate
    
  //   //TODO Any, All
  // }
*/