package zio.sql.table

import zio.schema._
import zio.sql.macros.TableSchema
import scala.collection.immutable
import zio.sql.macros.IsNotLiteral
import zio.sql._
import zio.sql.utils.TrailingUnitNormalizer

import zio.sql.expr.Expr
import zio.sql.select._

import zio.sql.utils.Pluralize

sealed trait Table { self =>
  protected[sql] type TableType

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

  /**
    * Creates a table descripton from the Schema of T. 
    * Table name is taken either from @name annotation or schema id type and pluralized.
    */
  // TODO do not allow CaseClass0 with macro
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

      protected[sql] val exprAccessorBuilder = new ExprAccessorBuilder(tableName)

      override protected[sql] type AllColumnIdentities = schema.Terms

      override protected[sql] type TableType = T

      override protected[sql] type ColumnsOut =
        schema.Accessors[exprAccessorBuilder.Lens, exprAccessorBuilder.Prism, exprAccessorBuilder.Traversal]

      override val columns: ColumnsOut = schema.makeAccessors(exprAccessorBuilder)

      override protected[sql] def all(implicit
        helper: SelectAllHelper[ColumnsOut, TableType]
      ): SelectBuilder[helper.F, TableType, helper.SelSet] =
        helper.apply(columns)

      override val name: String = tableName.toLowerCase()
    }

  def convertToSnakeCase(name: String): String = {
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
    schema.annotations.collectFirst { case TableNameAnnotation.name(name) => name } match {
      case Some(name) if raw"[A-Za-z_][A-Za-z0-9_]*".r.pattern.matcher(name).matches() => Some(name)
      case _                                                                           => None
    }

  class JoinBuilder[A, B](joinType: JoinType, left: Table.Aux[A], right: Table.Aux[B]) {
    def on[F](expr: Expr[F, A with B, Boolean])(implicit ev: IsNotLiteral[F]): Table.Aux[A with B] =
      Joined(joinType, left, right, expr)
  }

  type Aux[A] = Table { type TableType = A }

  type WithColumnsOut[A, ColumnsOut0] = Table {
    type TabelType  = A
    type ColumnsOut = ColumnsOut0
  }

  sealed trait Source extends Table {
    protected[sql] type AllColumnIdentities

    val name: String

    protected[sql] type ColumnsOut

    val columns: ColumnsOut

    protected[sql] def all(implicit
      helper: SelectAllHelper[ColumnsOut, TableType]
    ): SelectBuilder[helper.F, TableType, helper.SelSet]
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

  final case class Joined[FF, A, B](
    joinType: JoinType,
    left: Table.Aux[A],
    right: Table.Aux[B],
    on: Expr[FF, A with B, Boolean]
  ) extends Table {

    override type TableType = left.TableType with right.TableType
  }

  final case class DerivedTable[CO, +Out, +R <: Read.WithReprs[Out, CO], Source](read: R, name: String) extends Table {
    self =>
    type ColumnsOut = CO

    override type TableType = Source

    def columns(implicit normalizer: TrailingUnitNormalizer[CO]): normalizer.Out =
      normalizer.apply(read.columns(name))
  }

  final case class DialectSpecificTable[A](tableExtension: TableExtension[A]) extends Table {

    override type TableType = A
  }

  trait TableExtension[A]
}
