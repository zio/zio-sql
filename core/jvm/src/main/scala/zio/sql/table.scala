package zio.sql

import zio.Chunk

import java.time._
import java.util.UUID

trait TableModule { self: ExprModule with SelectModule =>

  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]
    type Append[That <: ColumnSet] <: ColumnSet

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

    type ConsAux[A, B <: ColumnSet, ColumnsRepr0[_]] = ColumnSet.Cons[A, B] {
      type ColumnsRepr[C] = ColumnsRepr0[C]
    }

    type Aux[ColumnsRepr0] = ColumnSet {
      type ColumnsRepr = ColumnsRepr0
    }

    case object Empty extends ColumnSet {
      override type ColumnsRepr[T]            = Unit
      override type Append[That <: ColumnSet] = That

      override def ++[That <: ColumnSet](that: That): Append[That] = that

      override def columnsUntyped: List[Column.Untyped] = Nil

      override def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] = ()

      override def contains[A](column: Column[A]): Boolean = false
    }

    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>

      override type ColumnsRepr[T]            = (Expr[Features.Source, T, A], tail.ColumnsRepr[T])
      override type Append[That <: ColumnSet] = Cons[A, tail.Append[That]]

      override def ++[That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)

      override def columnsUntyped: List[Column.Untyped] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Aux_[ColumnsRepr, A, B] =
        new Table.Source {
          override type ColumnHead = A
          override type ColumnTail = B

          override val name: TableName                     = name0
          override val columnSchema: ColumnSchema[A :*: B] = ColumnSchema(self)

          override val columnSet: ColumnSet.ConsAux[ColumnHead, ColumnTail, ColumnsRepr] = self

          override def columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
            def toExpr[A](column: Column[A]): Expr[Features.Source, TableType, A] = Expr.Source(name0, column)
          }
        }

      override def makeColumns[T](columnToExpr: ColumnToExpr[T]): ColumnsRepr[T] =
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

    val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail]

    def columnToExpr: ColumnToExpr[TableType]
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
        def toExpr[C](column: Column[C]): Expr[Features.Source, TableType, C] =
          if (left.columnSet.contains(column))
            left.columnToExpr.toExpr(column)
          else
            right.columnToExpr.toExpr(column)
      }
    }

    sealed case class DerivedTable[+Out, +R <: Read[Out]](read: R, name: TableName) extends Table { self =>

      override type ColumnHead = read.ColumnHead
      override type ColumnTail = read.ColumnTail

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] = read.columnSet

      override def columnToExpr: ColumnToExpr[TableType] = new ColumnToExpr[TableType] {
        def toExpr[A](column: Column[A]): Expr[Features.Source, TableType, A] =
          Expr.Source(name, column)
      }
    }

    sealed case class DialectSpecificTable[A](tableExtension: TableExtension[A]) extends Table {

      override type TableType = A

      override type ColumnHead = tableExtension.ColumnHead
      override type ColumnTail = tableExtension.ColumnTail

      override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] = tableExtension.columnSet

      override def columnToExpr: ColumnToExpr[TableType] = tableExtension.columnToExpr
    }

    trait TableEx[A] {

      type ColumnHead
      type ColumnTail <: ColumnSet

      def columnSet: ColumnSet.Cons[ColumnHead, ColumnTail]

      def columnToExpr: ColumnToExpr[A]
    }
  }

  type TableExtension[A] <: Table.TableEx[A]

}
