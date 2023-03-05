package zio.sql.macros 

sealed trait HavingIsSound[AllF, GroupByF, HavingF]

object HavingIsSound {

  final case class HavingCanBeCalled[AllF, GroupByF, HavingF]() extends HavingIsSound[AllF, GroupByF, HavingF]

  implicit def materializeHavingIsSound[AllF, GroupByF, HavingF]: HavingIsSound[AllF, GroupByF, HavingF] = HavingCanBeCalled[AllF, GroupByF, HavingF]()

}