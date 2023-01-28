package zio.sql.macros 

sealed trait WhereIsSound[WhereF, GroupByF]

object WhereIsSound {
  final case class WhereCanBeCalled[WhereF, GroupByF]() extends WhereIsSound[WhereF, GroupByF]

  implicit def materializeWhereIsSound[WhereF, GroupByF]: WhereIsSound[WhereF, GroupByF] = 
    WhereCanBeCalled[WhereF, GroupByF]()
}