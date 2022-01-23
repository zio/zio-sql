package zio.sql

import scala.annotation.implicitNotFound

trait FeaturesModule {

  type :||:[A, B] = Features.Union[A, B]

  object Features extends PartialAggregationLowerPrio {
    type Aggregated[_]
    type Union[_, _]
    type Source[_]
    //TODO make Derived and Join tables return Expr of type "Derived" when .columns is called
    type Derived
    type Literal
    type Function0

    sealed trait IsNotAggregated[A] 
    object IsNotAggregated {
      implicit def UnionIsNotAgregated[A: IsNotAggregated, B: IsNotAggregated]: IsNotAggregated[Union[A, B]] = 
        new IsNotAggregated[Union[A, B]] {}

      implicit def SourceIsNotAggregated[A]: IsNotAggregated[Source[A]] = 
        new IsNotAggregated[Source[A]] {}

      implicit val DerivedIsNotAggregated: IsNotAggregated[Derived] = 
        new IsNotAggregated[Derived] {}

      implicit val LiteralIsNotAggregated: IsNotAggregated[Literal] = 
        new IsNotAggregated[Literal] {}

      implicit val Function0IsNotAggregated: IsNotAggregated[Function0] = 
        new IsNotAggregated[Function0] {}
    }

    sealed trait IsFullyAggregated[A] 

    object IsFullyAggregated {
      def apply[A](implicit is: IsFullyAggregated[A]): IsFullyAggregated[A] = is

      implicit def AggregatedIsAggregated[A]: IsFullyAggregated[Aggregated[A]] = new IsFullyAggregated[Aggregated[A]] {}

      implicit def UnionIsAggregated[A: IsFullyAggregated, B: IsFullyAggregated]: IsFullyAggregated[Union[A, B]] =
        new IsFullyAggregated[Union[A, B]] {}
    }

    @implicitNotFound("You can only use this function on a column in the source table")
    sealed trait IsSource[A]

    object IsSource {
      implicit def isSource[ColumnIdentity]: IsSource[Source[ColumnIdentity]] = new IsSource[Source[ColumnIdentity]] {}
    }
  }

  trait PartialAggregationLowerPrio {
    sealed trait IsPartiallyAggregated[A]

    object IsPartiallyAggregated {
      
      def apply[A](implicit is: IsPartiallyAggregated[A]): IsPartiallyAggregated[A] = is

      implicit def AggregatedIsAggregated[A]: IsPartiallyAggregated[Features.Aggregated[A]] = new IsPartiallyAggregated[Features.Aggregated[A]] {}

      implicit def UnionIsAggregatedInB[A, B](implicit instB: IsPartiallyAggregated[B]): IsPartiallyAggregated[Features.Union[A, B]] =
        new IsPartiallyAggregated[Features.Union[A, B]] {}

      implicit def UnionIsAggregatedInA[A, B](implicit instB: IsPartiallyAggregated[A]): IsPartiallyAggregated[Features.Union[A, B]] =
        new IsPartiallyAggregated[Features.Union[A, B]] {}
    }
  }
}
