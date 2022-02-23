package zio.sql

import scala.annotation.implicitNotFound

trait FeaturesModule {

  type :||:[A, B] = Features.Union[A, B]

  object Features {
    type Aggregated[_]
    type Union[_, _]
    type Source[ColumnName, TableType]
    type Literal
    type Function0
    type Derived

    sealed trait IsNotAggregated[A]
    object IsNotAggregated {
      implicit def UnionIsNotAgregated[A: IsNotAggregated, B: IsNotAggregated]: IsNotAggregated[Union[A, B]] =
        new IsNotAggregated[Union[A, B]] {}

      implicit def SourceIsNotAggregated[ColumnName, TableType]: IsNotAggregated[Source[ColumnName, TableType]] =
        new IsNotAggregated[Source[ColumnName, TableType]] {}

      implicit val LiteralIsNotAggregated: IsNotAggregated[Literal] =
        new IsNotAggregated[Literal] {}

      implicit val DerivedIsNotAggregated: IsNotAggregated[Derived] =
        new IsNotAggregated[Derived] {}

      implicit val Function0IsNotAggregated: IsNotAggregated[Function0] =
        new IsNotAggregated[Function0] {}
    }

    sealed trait IsFullyAggregated[A]

    object IsFullyAggregated {
      def apply[A](implicit is: IsFullyAggregated[A]): IsFullyAggregated[A] = is

      implicit def AggregatedIsAggregated[A]: IsFullyAggregated[Aggregated[A]] = new IsFullyAggregated[Aggregated[A]] {}

      implicit val LiteralIsAggregated: IsFullyAggregated[Literal] = new IsFullyAggregated[Literal] {}

      implicit def UnionIsAggregated[A: IsFullyAggregated, B: IsFullyAggregated]: IsFullyAggregated[Union[A, B]] =
        new IsFullyAggregated[Union[A, B]] {}
    }

    @implicitNotFound("You can only use this function on a column in the source table")
    sealed trait IsSource[A]

    object IsSource {
      implicit def isSource[ColumnName, TableType]: IsSource[Source[ColumnName, TableType]] =
        new IsSource[Source[ColumnName, TableType]] {}
    }

    sealed trait IsPartiallyAggregated[A] {
      type Unaggregated
    }

    object IsPartiallyAggregated extends IsPartiallyAggregatedLowPriorityImplicits {

      type WithRemainder[F, R] = IsPartiallyAggregated[F] {
        type Unaggregated = R
      }

      def apply[A](implicit is: IsPartiallyAggregated[A]): IsPartiallyAggregated.WithRemainder[A, is.Unaggregated] = is

      implicit def AggregatedIsAggregated[A]: IsPartiallyAggregated.WithRemainder[Aggregated[A], Any] =
        new IsPartiallyAggregated[Aggregated[A]] {
          override type Unaggregated = Any
        }

      implicit def UnionIsAggregated[A, B](implicit
        inA: IsPartiallyAggregated[A],
        inB: IsPartiallyAggregated[B]
      ): IsPartiallyAggregated.WithRemainder[Union[A, B], inA.Unaggregated with inB.Unaggregated] =
        new IsPartiallyAggregated[Union[A, B]] {
          override type Unaggregated = inA.Unaggregated with inB.Unaggregated
        }

      implicit val LiteralIsAggregated: IsPartiallyAggregated.WithRemainder[Literal, Any] =
        new IsPartiallyAggregated[Literal] {
          override type Unaggregated = Any
        }

      implicit val DerivedIsAggregated: IsPartiallyAggregated.WithRemainder[Derived, Any] =
        new IsPartiallyAggregated[Derived] {
          override type Unaggregated = Any
        }

      implicit val FunctionIsAggregated: IsPartiallyAggregated.WithRemainder[Function0, Any] =
        new IsPartiallyAggregated[Function0] {
          override type Unaggregated = Any
        }
    }

    trait IsPartiallyAggregatedLowPriorityImplicits {
      implicit def SourceIsAggregated[ColumnName, TableType]: IsPartiallyAggregated.WithRemainder[
        Features.Source[ColumnName, TableType],
        Features.Source[ColumnName, TableType]
      ] =
        new IsPartiallyAggregated[Features.Source[ColumnName, TableType]] {
          override type Unaggregated = Features.Source[ColumnName, TableType]
        }
    }
  }
}
