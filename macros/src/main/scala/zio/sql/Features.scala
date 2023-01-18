package zio.sql

import scala.annotation.implicitNotFound

object Features {

  type Aggregated[_]
  type Source[ColumnName, TableType]
  type Literal
  type Function0
  type Derived

  @implicitNotFound("You can only use this function on a column in the source table")
  sealed trait IsSource[-A]

  object IsSource {
    implicit def isSource[ColumnName, TableType]: IsSource[Source[ColumnName, TableType]] =
      new IsSource[Source[ColumnName, TableType]] {}
  }
}
