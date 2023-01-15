package zio.sql

import scala.annotation.implicitNotFound

object Features {
  //type :||:[A, B] = Union[A, B]

  type Aggregated[_]
  //type Union[_, _]
  type Source[ColumnName, TableType]
  type Literal
  type Function0
  type Derived

  // @implicitNotFound("Use === instead of == when comparing two Exprs")
  // sealed trait IsNotLiteral[A]
  // object IsNotLiteral extends NotLiteralLowPrio {
  //   implicit def AggregatedIsNotLiteral[A](implicit ev: IsNotLiteral[A]): IsNotLiteral[Aggregated[A]] =
  //     new IsNotLiteral[Aggregated[A]] {}

  //   implicit def UnionIsNotLiteral[A, B](implicit
  //     ev1: IsNotLiteral[A],
  //     ev2: IsNotLiteral[B]
  //   ): IsNotLiteral[Union[A, B]] =
  //     new IsNotLiteral[Union[A, B]] {}



  //   implicit def SourceIsNotLiteral[ColumnName, TableType]: IsNotLiteral[Source[ColumnName, TableType]] =
  //     new IsNotLiteral[Source[ColumnName, TableType]] {}

  //   implicit val DerivedIsNotLiteral: IsNotLiteral[Derived] =
  //     new IsNotLiteral[Derived] {}

  //   implicit val Function0IsNotLiteral: IsNotLiteral[Function0] =
  //     new IsNotLiteral[Function0] {}
  // }

  // diverging implicit converstion
  // trait NotLiteralLowPrio {
  //   implicit def IntersectionIsNotLiteral[A, B](implicit
  //     ev1: IsNotLiteral[A],
  //     ev2: IsNotLiteral[B]
  //   ): IsNotLiteral[A with B] =
  //     new IsNotLiteral[A with B] {}
  // }

  @implicitNotFound("You can only use this function on a column in the source table")
  sealed trait IsSource[-A]

  object IsSource {
    implicit def isSource[ColumnName, TableType]: IsSource[Source[ColumnName, TableType]] =
      new IsSource[Source[ColumnName, TableType]] {}
  }
}
