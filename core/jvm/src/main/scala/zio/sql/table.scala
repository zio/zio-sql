package zio.sql

import zio.schema._
import zio.sql.macros.TableSchema
import scala.annotation.StaticAnnotation

object TableAnnotation {

  case class name(name: String) extends StaticAnnotation
}

trait TableModule { self: ExprModule with SelectModule with UtilsModule with SelectUtilsModule =>

  // naming conventions -> OrderOrigin => order_origins -> this one  
  def defineTableSmart[Z](implicit schema: Schema.Record[Z], tableLike: TableSchema[Z]): Table.Source.WithColumnsOut[
    schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal],
    Z,
    schema.Accessors[
      allColumnIdentitiesBuilder.Lens,
      allColumnIdentitiesBuilder.Prism,
      allColumnIdentitiesBuilder.Traversal
    ]
  ] = {
    val tableNameValid = raw"[A-Za-z_][A-Za-z0-9_]*".r

    val tableName = schema.annotations
      .collectFirst { case TableAnnotation.name(name) => name } match {
      case Some(name) if tableNameValid.pattern.matcher(name).matches() => name
      case _                                          => {
        // TODO properly pluralize recordSchema.id.name
        schema.id.name
      }
    }

    defineTable(tableName)
  }

  def defineTable[Z](implicit schema: Schema.Record[Z], tableLike: TableSchema[Z]): Table.Source.WithColumnsOut[
    schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal],
    Z,
    schema.Accessors[
      allColumnIdentitiesBuilder.Lens,
      allColumnIdentitiesBuilder.Prism,
      allColumnIdentitiesBuilder.Traversal
    ]
  ] = defineTable(schema.id.name)

  def defineTable[Z](
    tableName: String
  )(implicit schema: Schema.Record[Z], tableLike: TableSchema[Z]): Table.Source.WithColumnsOut[
    schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal],
    Z,
    schema.Accessors[
      allColumnIdentitiesBuilder.Lens,
      allColumnIdentitiesBuilder.Prism,
      allColumnIdentitiesBuilder.Traversal
    ]
  ] =
    new Table.Source {

      override type AllColumnIdentities =
        schema.Accessors[
          allColumnIdentitiesBuilder.Lens,
          allColumnIdentitiesBuilder.Prism,
          allColumnIdentitiesBuilder.Traversal
        ]

      override type TableType = Z

      override type ColumnsOut =
        schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal]

      // type Intersection[A, B] = A with B
      // in zio schema makeAccessorsWith[Intersection]()
      // def makeAccessorsWith[F[_, _]]
      // TODO we need to do this becouse order of insert does not matter
      override val columns: ColumnsOut = schema.makeAccessors(exprAccessorBuilder)

      override val name: TableName = convertToSnakeCase(tableName).toLowerCase()
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

  val allColumnIdentitiesBuilder = new AccessorBuilder {
    override type Lens[F, S, A] = F

    override type Prism[F, S, A] = Unit

    override type Traversal[S, A] = Unit

    def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[F, S, A] =
      convertToSnakeCase(term.label).asInstanceOf[F]

    def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[F, S, A] = ()

    def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()
  }

  val exprAccessorBuilder = new AccessorBuilder {
    override type Lens[F, S, A] = Expr[Features.Source[F, S], S, A]

    override type Prism[F, S, A] = Unit

    override type Traversal[S, A] = Unit

    def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[F, S, A] = {
      implicit val typeTag = deriveTypeTag(term.schema).get

      val column: Column.Aux[A, F] = Column.Named[A, F](convertToSnakeCase(term.label))

      // TODO what if user defined its own name ???
      Expr.Source(convertToSnakeCase(product.id.name).toLowerCase(), column)
    }

    def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[F, S, A] = ()

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
      // TODO on(expr1 == expr2) yields false, which may be surprising
      // https://github.com/zio/zio-sql/issues/587
      // idea -> restrict F so its union or anything just not literal
      def on[F](expr: Expr[F, A with B, Boolean]): Table.Aux[A with B] =
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

      type WithColumnsOut[ColumnsOut0, A, AllColumnIdentities0] = Table.Source {
        type TableType = A

        type ColumnsOut = ColumnsOut0

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
    }

    sealed case class DerivedTable[CO, +Out, +R <: Read.WithReprs[Out, CO], Source](read: R, name: TableName) extends Table {
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
      case StandardType.BigDecimalType        => Some(TypeTag.TBigDecimal)
      case StandardType.BoolType              => Some(TypeTag.TBoolean)
      case StandardType.ByteType              => Some(TypeTag.TByte)
      case StandardType.BinaryType            => Some(TypeTag.TByteArray)
      case StandardType.CharType              => Some(TypeTag.TChar)
      case StandardType.DoubleType            => Some(TypeTag.TDouble)
      case StandardType.FloatType             => Some(TypeTag.TFloat)
      case StandardType.InstantType(_)        => Some(TypeTag.TInstant)
      case StandardType.IntType               => Some(TypeTag.TInt)
      case StandardType.BigIntegerType        => None
      case StandardType.LocalDateType(_)      => Some(TypeTag.TLocalDate)
      case StandardType.LocalDateTimeType(_)  => Some(TypeTag.TLocalDateTime)
      case StandardType.OffsetTimeType(_)     => Some(TypeTag.TOffsetTime)
      case StandardType.LocalTimeType(_)      => Some(TypeTag.TLocalTime)
      case StandardType.LongType              => Some(TypeTag.TLong)
      case StandardType.OffsetDateTimeType(_) => Some(TypeTag.TOffsetDateTime)
      case StandardType.ShortType             => Some(TypeTag.TShort)
      case StandardType.StringType            => Some(TypeTag.TString)
      case StandardType.UUIDType              => Some(TypeTag.TUUID)
      case StandardType.ZonedDateTimeType(_)  => Some(TypeTag.TZonedDateTime)
      // TODO do we need to support any other types in SQL ?
      case StandardType.ZoneOffsetType        => None
      case StandardType.DurationType          => None
      case StandardType.YearType              => None
      case StandardType.MonthType             => None
      case StandardType.MonthDayType          => None
      case StandardType.ZoneIdType            => None
      case StandardType.PeriodType            => None
      case StandardType.YearMonthType         => None
      case StandardType.DayOfWeekType         => None
      case StandardType.UnitType              => None
    }

  def deriveTypeTag[A](opSchema: Schema.Optional[A]): Option[TypeTag[Option[A]]] =
    opSchema.codec match {
      case Schema.Primitive(standardType, _) =>
        implicit val notNullTypeTag = deriveTypeTag(standardType).get

        Some(TypeTag.option[A])
      case _                                 => None
    }

  // make zio schema  TypeTag of A available out of Schmea[A]
  def deriveTypeTag[A](fieldSchema: Schema[A]): Option[TypeTag[A]] =
    fieldSchema match {
      case s: Schema.Optional[_]             => deriveTypeTag(s)
      case s: Schema.Lazy[A]                 => deriveTypeTag(s.schema)
      case Schema.Primitive(standardType, _) => deriveTypeTag(standardType)
      case _: Schema.Transform[_, _, _]      => {
        
        // if (fieldSchema.isInstanceOf[Schema[BigDecimal]]) {
        //   Some(TypeTag.TScalaBigDecimal.asInstanceOf[TypeTag[A]])
        // } 
        // else None
        // TODO fix
        Some(TypeTag.TScalaBigDecimal.asInstanceOf[TypeTag[A]])
      }
      case _                                 => None
    }

}