package zio.sql

import zio.Chunk

import java.time._
import java.util.UUID

trait TableModule { self: ExprModule with SelectModule =>

  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]
    type Append[That <: ColumnSet] <: ColumnSet
    type Size <: ColumnCount

    def ++[That <: ColumnSet](that: That): Append[That]

    def columnsUntyped: List[Column.Untyped]

    // TODO figure out how to make Column equality well defined
    def contains[A](column: Column[A]): Boolean

    def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T]
  }

  object ColumnSet {
    type Empty                  = Empty.type
    type :*:[A, B <: ColumnSet] = Cons[A, B]
    type Singleton[A]           = Cons[A, Empty]
    import ColumnCount._

    type ConsAux[A, B <: ColumnSet, ColumnsRepr0[_]] = ColumnSet.Cons[A, B] {
      type ColumnsRepr[C] = ColumnsRepr0[C]
    }

    type Aux[ColumnsRepr0] = ColumnSet {
      type ColumnsRepr = ColumnsRepr0
    }

    case object Empty extends ColumnSet {
      override type ColumnsRepr[T]            = Unit
      override type Append[That <: ColumnSet] = That

      override type Size = _0

      override def ++[That <: ColumnSet](that: That): Append[That] = that

      override def columnsUntyped: List[Column.Untyped] = Nil

      override def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] = ()

      override def contains[A](column: Column[A]): Boolean = false
    }

    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>

      override type ColumnsRepr[T]            = (Expr[Features.Source, T, A], tail.ColumnsRepr[T])
      override type Append[That <: ColumnSet] = Cons[A, tail.Append[That]]

      override type Size = Succ[tail.Size]

      override def ++[That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)

      override def columnsUntyped: List[Column.Untyped] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Aux_[ColumnsRepr, A, B] =
        new Table.Source {
          override type ColumnHead = A
          override type ColumnTail = B

          override type Size = columnSet.Size

          override val name: TableName                     = name0
          override val columnSchema: ColumnSchema[A :*: B] = ColumnSchema(self)

          override val columnSet: ColumnSet.ConsAux[ColumnHead, ColumnTail, ColumnsRepr] = self

          override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
            def toExpr[A](column: Column[A]): Expr[Features.Source, TableType, A] = Expr.Source(name0, column)
          }
        }

      def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] =
        (columnToExpr.toExpr(head), tail.makeColumns(columnToExpr))

      override def contains[A](column: Column[A]): Boolean =
        head == column || tail.contains(column)
    }

    def bigDecimal(name: String): Singleton[BigDecimal]         = singleton[BigDecimal](name)
    def boolean(name: String): Singleton[Boolean]               = singleton[Boolean](name)
    def byteArray(name: String): Singleton[Chunk[Byte]]         = singleton[Chunk[Byte]](name)
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
    def singleton[A: TypeTag](name: String): Singleton[A]       = Cons(Column.Named[A](name), Empty)
    def string(name: String): Singleton[String]                 = singleton[String](name)
    def uuid(name: String): Singleton[UUID]                     = singleton[UUID](name)
    def zonedDateTime(name: String): Singleton[ZonedDateTime]   = singleton[ZonedDateTime](name)
  }

  object :*: {
    def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
  }

  sealed trait Column[+A] {
    def typeTag: TypeTag[A]
  }

  object Column {
    sealed case class Named[A: TypeTag](columnName: String) extends Column[A] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }

    sealed case class Indexed[A: TypeTag]() extends Column[A] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }

    type Untyped = Column[_]
  }

  sealed trait JoinType

  object JoinType {
    case object Inner      extends JoinType
    case object LeftOuter  extends JoinType
    case object RightOuter extends JoinType
    case object FullOuter  extends JoinType
  }

  trait ColumnToExpr[TableType] {
    def toExpr[A](column: Column[A]): Expr[Features.Source, TableType, A]
  }

  sealed trait Table { self =>
    type TableType

    type Cols = ColumnSet.Cons[ColumnHead, ColumnTail]

    type ColumnHead
    type ColumnTail <: ColumnSet
    type Size = columnSet.Size

    final def fullOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.FullOuter, self, that)

    final def join[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.Inner, self, that)

    final def leftOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.LeftOuter, self, that)

    final def rightOuter[That](that: Table.Aux[That]): Table.JoinBuilder[self.TableType, That] =
      new Table.JoinBuilder[self.TableType, That](JoinType.RightOuter, self, that)

    final val subselect: SubselectPartiallyApplied[TableType] = new SubselectPartiallyApplied[TableType]

    final def columns = columnSet.makeColumns[TableType](columnToExpr)

    //final val columnSize : Size = columnSet.size

    val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail]

    val columnToExpr: ColumnToExpr[TableType]
  }

  object Table {

    class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {
      def on[F](expr: Expr[F, A with B, Boolean]): Table.Aux[A with B] =
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    type Aux_[ColumnsRepr[_], A, B <: ColumnSet] = Table.Source {
      type ColumnHead = A
      type ColumnTail = B
      def columnSet: ColumnSet.ConsAux[A, B, ColumnsRepr]
    }

    // Absence of "Insanity" trait causes following known problems:
    // * in db modules
    //     - The outer reference in this type test cannot be checked at run time?!
    //       case sourceTable: self.Table.Source    =>
    // * compiletime error in updating of table like
    //     - update(table).set(age, age + 2)...
    trait Insanity {
      def ahhhhhhhhhhhhh[A]: A
    }

    sealed trait Source extends Table with Insanity {

      val name: TableName
      val columnSchema: ColumnSchema[Cols]

      override def ahhhhhhhhhhhhh[A]: A = ??? //don't remove or it'll break
    }

    object Source {
      type Aux[A, Size0] = Table.Source {
        type TableType = A
        type Size      = Size0
      }
    }

    sealed case class Joined[FF, A, B](
      joinType: JoinType,
      left: Table.Aux[A],
      right: Table.Aux[B],
      on: Expr[FF, A with B, Boolean]
    ) extends Table {

      override type TableType = left.TableType with right.TableType

      override type ColumnHead = left.ColumnHead
      override type ColumnTail =
        left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail]]

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] =
        left.columnSet ++ right.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
        def toExpr[C](column: Column[C]) =
          if (left.columnSet.contains(column))
            left.columnToExpr.toExpr(column)
          else
            right.columnToExpr.toExpr(column)
      }
    }

    sealed case class DerivedTable[+R <: Read[_]](read: R, name: TableName) extends Table { self =>

      override type ColumnHead = read.ColumnHead
      override type ColumnTail = read.ColumnTail

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] = read.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
        def toExpr[A](column: Column[A]): Expr[Features.Source, TableType, A] =
          Expr.Source(name, column)
      }
    }

    sealed case class DialectSpecificTable[A](tableExtension: TableExtension[A]) extends Table {

      override type TableType = A

      override type ColumnHead = tableExtension.ColumnHead
      override type ColumnTail = tableExtension.ColumnTail

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] = tableExtension.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = tableExtension.columnToExpr
    }

    trait TableEx[A] {

      type ColumnHead
      type ColumnTail <: ColumnSet

      def columnSet: ColumnSet.Cons[ColumnHead, ColumnTail]

      def columnToExpr: ColumnToExpr[A]
    }
  }

  type TableExtension[A] <: Table.TableEx[A]

  sealed trait ColumnCount {

    type Appended[That <: ColumnCount] <: ColumnCount

    def add[That <: ColumnCount](that: That): Appended[That]
  }
  object ColumnCount {
    case object Zero extends ColumnCount {
      override type Appended[That <: ColumnCount] = That

      override def add[That <: ColumnCount](that: That): Appended[That] = that
    }

    sealed case class Succ[C <: ColumnCount](c: C) extends ColumnCount {
      override type Appended[That <: ColumnCount] = Succ[c.Appended[That]]

      override def add[That <: ColumnCount](that: That): Appended[That] = Succ(c.add(that))
    }

    type _0 = Zero.type
    val _0 = Zero
    // type _1 = Succ[_0]
    // val _1 = Succ(_0)
    // type _2 = Succ[_1]
    // val _2 = Succ(_1)
    // type _3 = Succ[_2]
    // val _3 = Succ(_2)
    // type _4 = Succ[_3]
    // val _4 = Succ(_3)
    // type _5 = Succ[_4]
    // val _5 = Succ(_4)
    // type _6 = Succ[_5]
    // val _6 = Succ(_5)
    // type _7 = Succ[_6]
    // val  _7 = Succ(_6)
    // type _8 = Succ[_7]
    // val  _8 = Succ(_7)
    // type _9 = Succ[_8]
    // val  _9 = Succ(_8)
    // type _10 = Succ[_9]
    // val  _10 = Succ(_9)
    // type _11 = Succ[_10]
    // val  _11 = Succ(_10)
    // type _12 = Succ[_11]
    // val  _12 = Succ(_11)
    // type _13 = Succ[_12]
    // val  _13 = Succ(_12)
    // type _14 = Succ[_13]
    // val  _14 = Succ(_13)
    // type _15 = Succ[_14]
    // val  _15 = Succ(_14)
    // type _16 = Succ[_15]
    // val  _16 = Succ(_15)
    // type _17 = Succ[_16]
    // val  _17 = Succ(_16)
    // type _18 = Succ[_17]
    // val  _18 = Succ(_17)
    // type _19 = Succ[_18]
    // val  _19 = Succ(_18)
    // type _20 = Succ[_19]
    // val  _20 = Succ(_19)
    // type _21 = Succ[_20]
    // val  _21 = Succ(_20)
    // type _22 = Succ[_21]
    // val  _22 = Succ(_21)
  }
}
