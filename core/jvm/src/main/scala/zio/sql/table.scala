package zio.sql

import zio.schema._
import zio.sql.macros.TableSchema
import scala.annotation.StaticAnnotation
import scala.collection.immutable

object TableAnnotation {
  case class name(name: String) extends StaticAnnotation
}

trait TableModule { self: ExprModule with SelectModule with UtilsModule with SelectUtilsModule =>

  type Lens[F, S, A] = Expr[Features.Source[F, S], S, A]

  type Prism[F, S, A] = Unit

  type Traversal[S, A] = Unit

  /**
    * Creates a table descripton from the Schema of T. 
    * Table name is taken either from @name annotation or schema id type and pluralized.
    */
  def defineTableSmart[T](implicit
    schema: Schema.Record[T],
    tableLike: TableSchema[T]
  ): Table.Source.WithTableDetails[schema.Terms, T, schema.Accessors[Lens, Prism, Traversal]] = {
    val tableName = extractAnnotationName(schema) match {
      case Some(name) => name
      case None       =>
        pluralize(
          convertToSnakeCase(schema.id.name)
            .split("_")
            .toList
        )
    }

    defineTable(tableName)
  }

  /**
    * Creates a table descripton from the Schema of T. 
    * Table name is taken either from @name annotation or schema id type.
    */
  def defineTable[T](implicit
    schema: Schema.Record[T],
    tableLike: TableSchema[T]
  ): Table.Source.WithTableDetails[schema.Terms, T, schema.Accessors[Lens, Prism, Traversal]] = {
    val tableName = extractAnnotationName(schema) match {
      case Some(name) => name
      case None       => convertToSnakeCase(schema.id.name)
    }

    defineTable(tableName)
  }

  /**
    * Creates a table descripton from the Schema of T. 
    * Table name is explicitely provided.
    */
  def defineTable[T](
    tableName: String
  )(implicit
    schema: Schema.Record[T],
    tableLike: TableSchema[T]
  ): Table.Source.WithTableDetails[schema.Terms, T, schema.Accessors[Lens, Prism, Traversal]] =
    new Table.Source {

      val exprAccessorBuilder = new ExprAccessorBuilder(tableName)

      override type AllColumnIdentities = schema.Terms

      override type TableType = T

      override type ColumnsOut =
        schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal]

      override val columns: ColumnsOut = schema.makeAccessors(exprAccessorBuilder)

      override val name: TableName = tableName.toLowerCase()
    }

  private def convertToSnakeCase(name: String): String = {
    val temp = (name.head.toLower.toString + name.tail)
    temp.indexWhere(_.isUpper) match {
      case -1 => temp
      case i  =>
        val (prefix, suffix) = temp.splitAt(i)
        prefix + "_" + convertToSnakeCase(suffix)
    }
  }

  private def pluralize(names: List[String]): String =
    names match {
      case Nil                   => ""
      case head :: immutable.Nil => Pluralize.pluralize(head)
      case head :: next          => head + "_" + pluralize(next)
    }

  private def extractAnnotationName[T](schema: Schema.Record[T]): Option[String] =
    schema.annotations.collectFirst { case TableAnnotation.name(name) => name } match {
      case Some(name) if raw"[A-Za-z_][A-Za-z0-9_]*".r.pattern.matcher(name).matches() => Some(name)
      case _                                                                           => None
    }

  class ExprAccessorBuilder(name: TableName) extends AccessorBuilder {

    override type Lens[F, S, A] = Expr[Features.Source[F, S], S, A]

    override type Prism[F, S, A] = Unit

    override type Traversal[S, A] = Unit

    def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[S, A]): Expr[Features.Source[F, S], S, A] = {
      implicit val typeTag = deriveTypeTag(term.schema).get

      val column: Column.Aux[A, F] = Column.Named[A, F](convertToSnakeCase(term.name.toString()))

      Expr.Source(name, column)
    }

    def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[S, A]): Unit = ()

    def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()

  }

  sealed trait Column[+A] {
    type Identity
    def typeTag: TypeTag[A]

    def name: Option[String]

    def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity]
  }

  object Column {

    type Aux[+A0, Identity0] = Column[A0] {
      type Identity = Identity0
    }

    sealed case class Named[A: TypeTag, ColumnIdentity](columnName: String) extends Column[A] {
      override type Identity = ColumnIdentity

      override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

      override def name = Some(columnName)

      override def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity] =
        Column.Named[Option[A1], ColumnIdentity](columnName)
    }

    sealed case class Indexed[A: TypeTag, ColumnIdentity]() extends Column[A] {

      override type Identity = ColumnIdentity

      override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

      override def name = None

      override def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity] =
        Column.Indexed[Option[A1], ColumnIdentity]()
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

    final val subselect: SubselectPartiallyApplied[TableType] = new SubselectPartiallyApplied[TableType]
  }

  object Table {

    class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {
      def on[F](expr: Expr[F, A with B, Boolean])(implicit ev: Features.IsNotLiteral[F]): Table.Aux[A with B] =
        Joined(joinType, left, right, expr)
    }

    type Aux[A] = Table { type TableType = A }

    type WithColumnsOut[A, ColumnsOut0] = Table {
      type TabelType  = A
      type ColumnsOut = ColumnsOut0
    }

    trait Insanity {
      def ahhhhhhhhhhhhh[A]: A
    }

    sealed trait Source extends Table with Insanity {
      type AllColumnIdentities

      val name: TableName

      type ColumnsOut

      val columns: ColumnsOut

      override def ahhhhhhhhhhhhh[A]: A = ??? // don't remove or it'll break
    }

    object Source {
      type Aux[A] = Table.Source {
        type TableType = A
      }

      type Aux_[A, AllColumnIdentities0] = Table.Source {
        type TableType           = A
        type AllColumnIdentities = AllColumnIdentities0
      }

      type WithTableDetails[AllColumnIdentities0, T, ColumnsOut0] = Table.Source {
        type AllColumnIdentities = AllColumnIdentities0
        type TableType           = T
        type ColumnsOut          = ColumnsOut0
      }
    }

    sealed case class Joined[FF, A, B](
      joinType: JoinType,
      left: Table.Aux[A],
      right: Table.Aux[B],
      on: Expr[FF, A with B, Boolean]
    ) extends Table {

      override type TableType = left.TableType with right.TableType
    }

    sealed case class DerivedTable[CO, +Out, +R <: Read.WithReprs[Out, CO], Source](read: R, name: TableName)
        extends Table {
      self =>
      type ColumnsOut = CO

      override type TableType = Source

      def columns(implicit normalizer: TrailingUnitNormalizer[CO]): normalizer.Out =
        normalizer.apply(read.columns(name))
    }

    sealed case class DialectSpecificTable[A](tableExtension: TableExtension[A]) extends Table {

      override type TableType = A
    }

    trait TableEx[A]
  }

  type TableExtension[A] <: Table.TableEx[A]

  def deriveTypeTag[A](standardType: StandardType[A]): Option[TypeTag.NotNull[A]] =
    standardType match {
      case StandardType.BigDecimalType     => Some(TypeTag.TBigDecimal)
      case StandardType.BoolType           => Some(TypeTag.TBoolean)
      case StandardType.ByteType           => Some(TypeTag.TByte)
      case StandardType.BinaryType         => Some(TypeTag.TByteArray)
      case StandardType.CharType           => Some(TypeTag.TChar)
      case StandardType.DoubleType         => Some(TypeTag.TDouble)
      case StandardType.FloatType          => Some(TypeTag.TFloat)
      case StandardType.InstantType        => Some(TypeTag.TInstant)
      case StandardType.IntType            => Some(TypeTag.TInt)
      case StandardType.LocalDateType      => Some(TypeTag.TLocalDate)
      case StandardType.LocalDateTimeType  => Some(TypeTag.TLocalDateTime)
      case StandardType.OffsetTimeType     => Some(TypeTag.TOffsetTime)
      case StandardType.LocalTimeType      => Some(TypeTag.TLocalTime)
      case StandardType.LongType           => Some(TypeTag.TLong)
      case StandardType.OffsetDateTimeType => Some(TypeTag.TOffsetDateTime)
      case StandardType.ShortType          => Some(TypeTag.TShort)
      case StandardType.StringType         => Some(TypeTag.TString)
      case StandardType.UUIDType           => Some(TypeTag.TUUID)
      case StandardType.ZonedDateTimeType  => Some(TypeTag.TZonedDateTime)
      // TODO What other types to support ?
      case StandardType.BigIntegerType     => None
      case StandardType.ZoneOffsetType     => None
      case StandardType.DurationType       => None
      case StandardType.YearType           => None
      case StandardType.MonthType          => None
      case StandardType.MonthDayType       => None
      case StandardType.ZoneIdType         => None
      case StandardType.PeriodType         => None
      case StandardType.YearMonthType      => None
      case StandardType.DayOfWeekType      => None
      case StandardType.UnitType           => None
    }

  def deriveTypeTag[A](opSchema: Schema.Optional[A]): Option[TypeTag[Option[A]]] =
    opSchema.schema match {
      case Schema.Primitive(standardType, _) =>
        implicit val notNullTypeTag = deriveTypeTag(standardType).get

        Some(TypeTag.option[A])
      case _                                 => None
    }

  def deriveTypeTag[A](fieldSchema: Schema[A]): Option[TypeTag[A]] =
    fieldSchema match {
      case s: Schema.Optional[_]                      => deriveTypeTag(s)
      case s: Schema.Lazy[A]                          => deriveTypeTag(s.schema)
      case Schema.Primitive(standardType, _)          => deriveTypeTag(standardType)
      case Schema.Sequence(elementSchema, _, _, _, _) =>
        elementSchema match {
          case Schema.Primitive(standardType, _) if (standardType == StandardType.ByteType) =>
            Some(TypeTag.TByteArray.asInstanceOf[TypeTag[A]])
          case _                                                                            => None
        }

      // TODO get TypeTag of A available out of Schema[A] and derive typetag from Schema.Transform
      case _: Schema.Transform[_, _, _]               => None
      case _                                          => None
    }
}
