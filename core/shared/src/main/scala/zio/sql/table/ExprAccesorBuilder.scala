package zio.sql.table

import zio.sql._
import zio.schema._
import zio.sql.typetag._

import zio.sql.expr.Expr

class ExprAccessorBuilder(name: String) extends AccessorBuilder {

  override type Lens[F, S, A] = Expr[Features.Source[F, S], S, A]

  override type Prism[F, S, A] = Unit

  override type Traversal[S, A] = Unit

  def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[S, A]): Expr[Features.Source[F, S], S, A] = {
    implicit val typeTag = TypeTag.deriveTypeTag(term.schema).get

    val column: Column.Aux[A, F] = Column.Named[A, F](Table.convertToSnakeCase(term.name.toString()))

    Expr.Source(name, column)
  }

  def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[S, A]): Unit = ()

  def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] = ()

}
