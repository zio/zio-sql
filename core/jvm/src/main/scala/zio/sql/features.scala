package zio.sql

import com.github.ghik.silencer.silent

import scala.annotation.implicitNotFound

trait FeaturesModule {

  type :||:[A, B] = Features.Union[A, B]

  object Features {
    type Aggregated[_]
    type Union[_, _]
    type Source
    type Literal
    type Function0

    sealed trait IsAggregated[A]

    object IsAggregated {
      def apply[A](implicit is: IsAggregated[A]): IsAggregated[A] = is

      implicit def AggregatedIsAggregated[A]: IsAggregated[Aggregated[A]] = new IsAggregated[Aggregated[A]] {}

      @silent
      implicit def UnionIsAggregated[A: IsAggregated, B: IsAggregated]: IsAggregated[Union[A, B]] =
        new IsAggregated[Union[A, B]] {}
    }

    @implicitNotFound("You can only use this function on a column in the source table")
    sealed trait IsSource[A]

    object IsSource {
      implicit case object SourceIsSource extends IsSource[Source]
    }
  }

}
