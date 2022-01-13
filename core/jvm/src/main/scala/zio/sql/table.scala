package zio.sql

import zio.Chunk

import java.time._
import java.util.UUID

trait TableModule { self: ExprModule with SelectModule =>

  sealed trait Singleton0[A] {
    type SingletonIdentity
  }

  sealed trait ColumnSet {
    type ColumnsRepr[T]
    type Append[That <: ColumnSet] <: ColumnSet
    type AllColumnIdentities

    def ++[That <: ColumnSet](that: That): Append[That]

    def columnsUntyped: List[Column.Untyped]

    // TODO figure out how to make Column equality well defined
    def contains[A](column: Column[A]): Boolean

    def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T]
  }

  object ColumnSet {

    type Empty                                = Empty.type
    type :*:[A, B <: ColumnSet, HeadIdentity] = Cons[A, B, HeadIdentity]
    type Singleton[A, ColumnIdentity]         = Cons[A, Empty, ColumnIdentity]

    type ConsAux[A, B <: ColumnSet, ColumnsRepr0[_], HeadIdentity] = ColumnSet.Cons[A, B, HeadIdentity] {
      type ColumnsRepr[C] = ColumnsRepr0[C]
    }

    type Aux[ColumnsRepr0] = ColumnSet {
      type ColumnsRepr = ColumnsRepr0
    }

    case object Empty extends ColumnSet {
      override type ColumnsRepr[T]            = Unit
      override type Append[That <: ColumnSet] = That

      override type AllColumnIdentities = Any

      override def ++[That <: ColumnSet](that: That): Append[That] = that

      override def columnsUntyped: List[Column.Untyped] = Nil

      override def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] = ()

      override def contains[A](column: Column[A]): Boolean = false
    }

    sealed case class Cons[A, B <: ColumnSet, HeadIdentity](head: Column.Aux[A, HeadIdentity], tail: B)
        extends ColumnSet { self =>

      override type ColumnsRepr[T] = (Expr[Features.Source[HeadIdentity], T, A], tail.ColumnsRepr[T])

      override type Append[That <: ColumnSet] = Cons[A, tail.Append[That], HeadIdentity]

      override def ++[That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)

      override type AllColumnIdentities = HeadIdentity with tail.AllColumnIdentities

      override def columnsUntyped: List[Column.Untyped] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Aux_[ColumnsRepr, A, B, AllColumnIdentities, HeadIdentity] =
        new Table.Source {
          override type ColumnHead = A
          override type ColumnTail = B

          override type HeadIdentity0 = HeadIdentity

          override type AllColumnIdentities = HeadIdentity with tail.AllColumnIdentities

          override val name: TableName = name0

          override val columnSet: ColumnSet.ConsAux[ColumnHead, ColumnTail, ColumnsRepr, HeadIdentity] = self

          override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
            def toExpr[A](column: Column[A]): Expr.Source[TableType, A, column.Identity] = Expr.Source(name0, column)
          }
        }

      def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] =
        (columnToExpr.toExpr(head), tail.makeColumns(columnToExpr))

      override def contains[A](column: Column[A]): Boolean =
        head == column || tail.contains(column)
    }

    def byteArray(name: String): Singleton[Chunk[Byte], name.type]         = singleton[Chunk[Byte], name.type](name)
    def bigDecimal(name: String): Singleton[BigDecimal, name.type]         = singleton[BigDecimal, name.type](name)
    def boolean(name: String): Singleton[Boolean, name.type]               = singleton[Boolean, name.type](name)
    def char(name: String): Singleton[Char, name.type]                     = singleton[Char, name.type](name)
    def double(name: String): Singleton[Double, name.type]                 = singleton[Double, name.type](name)
    def float(name: String): Singleton[Float, name.type]                   = singleton[Float, name.type](name)
    def instant(name: String): Singleton[Instant, name.type]               = singleton[Instant, name.type](name)
    def int(name: String): Singleton[Int, name.type]                       = singleton[Int, name.type](name)
    def localDate(name: String): Singleton[LocalDate, name.type]           = singleton[LocalDate, name.type](name)
    def localDateTime(name: String): Singleton[LocalDateTime, name.type]   = singleton[LocalDateTime, name.type](name)
    def localTime(name: String): Singleton[LocalTime, name.type]           = singleton[LocalTime, name.type](name)
    def long(name: String): Singleton[Long, name.type]                     = singleton[Long, name.type](name)
    def offsetDateTime(name: String): Singleton[OffsetDateTime, name.type] = singleton[OffsetDateTime, name.type](name)
    def offsetTime(name: String): Singleton[OffsetTime, name.type]         = singleton[OffsetTime, name.type](name)
    def short(name: String): Singleton[Short, name.type]                   = singleton[Short, name.type](name)
    def string(name: String): Singleton[String, name.type]                 = singleton[String, name.type](name)
    def uuid(name: String): Singleton[UUID, name.type]                     = singleton[UUID, name.type](name)
    def zonedDateTime(name: String): Singleton[ZonedDateTime, name.type]   = singleton[ZonedDateTime, name.type](name)

    def singleton[A: TypeTag, ColumnIdentity](name: String): Singleton[A, ColumnIdentity] =
      Cons(Column.Named[A, ColumnIdentity](name), Empty)
  }

  object :*: {
    def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
  }

  sealed trait Column[+A] {
    type Identity
    def typeTag: TypeTag[A]

    def name: Option[String]
  }

  object Column {

    type Aux[+A0, Identity0] = Column[A0] {
      type Identity = Identity0
    }

    sealed case class Named[A: TypeTag, ColumnIdentity](columnName: String) extends Column[A] {
      override type Identity = ColumnIdentity

      override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

      override def name = Some(columnName)
    }

    sealed case class Indexed[A: TypeTag, ColumnIdentity]() extends Column[A] {

      override type Identity = ColumnIdentity

      override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

      override def name = None
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
    def toExpr[A](column: Column[A]): Expr[Features.Source[column.Identity], TableType, A]
  }

  sealed trait Table { self =>
    type TableType

    type Cols = ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0]

    type HeadIdentity0

    type ColumnHead
    type ColumnTail <: ColumnSet

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

    val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0]

    val columnToExpr: ColumnToExpr[TableType]
  }

  object Table {

    class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {
      def on[F](expr: Expr[F, A with B, Boolean]): Table.Aux[A with B] =
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    type Aux_[ColumnsRepr[_], A, B <: ColumnSet, AllColumnIdentities0, HeadIdentity] = Table.Source {
      type ColumnHead          = A
      type ColumnTail          = B
      type AllColumnIdentities = AllColumnIdentities0

      type HeadIdentity0 = HeadIdentity
      val columnSet: ColumnSet.ConsAux[A, B, ColumnsRepr, HeadIdentity]
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

      type AllColumnIdentities

      val name: TableName

      override def ahhhhhhhhhhhhh[A]: A = ??? //don't remove or it'll break
    }

    object Source {
      type Aux[A] = Table.Source {
        type TableType = A
      }

      type Aux_[A, AllColumnIdentities0] = Table.Source {
        type TableType           = A
        type AllColumnIdentities = AllColumnIdentities0
      }
    }

    sealed case class Joined[FF, A, B](
      joinType: JoinType,
      left: Table.Aux[A],
      right: Table.Aux[B],
      on: Expr[FF, A with B, Boolean]
    ) extends Table {

      override type TableType = left.TableType with right.TableType

      override type HeadIdentity0 = left.HeadIdentity0

      override type ColumnHead = left.ColumnHead
      override type ColumnTail =
        left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail, right.HeadIdentity0]]

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] =
        left.columnSet ++ right.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
        def toExpr[C](column: Column[C]): Expr[Features.Source[column.Identity], A with B, C] =
          if (left.columnSet.contains(column))
            left.columnToExpr.toExpr(column)
          else
            right.columnToExpr.toExpr(column)
      }
    }

    sealed case class DerivedTable[+Out, +R <: Read[Out]](read: R, name: TableName) extends Table { self =>

      override type ColumnHead = read.ColumnHead
      override type ColumnTail = read.ColumnTail

      override type HeadIdentity0 = read.HeadIdentity

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] = read.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
        def toExpr[A](column: Column[A]): Expr[Features.Source[column.Identity], TableType, A] =
          Expr.Source(name, column)
      }
    }

    sealed case class DialectSpecificTable[A](tableExtension: TableExtension[A]) extends Table {

      override type TableType = A

      override type ColumnHead = tableExtension.ColumnHead
      override type ColumnTail = tableExtension.ColumnTail

      override type HeadIdentity0 = tableExtension.HeadIdentity0

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] = tableExtension.columnSet

      override val columnToExpr: ColumnToExpr[TableType] = tableExtension.columnToExpr
    }

    trait TableEx[A] {

      type ColumnHead
      type ColumnTail <: ColumnSet
      type HeadIdentity0

      def columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0]

      def columnToExpr: ColumnToExpr[A]
    }
  }

  type TableExtension[A] <: Table.TableEx[A]
}
