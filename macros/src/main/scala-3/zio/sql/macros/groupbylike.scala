package zio.sql.macros

sealed trait GroupByLike[All, Grouped]

object GroupByLike {

  final case class CanBeGrouped[All, Grouped]() extends GroupByLike[All, Grouped]

  implicit def createGroupByLike[All, Grouped]: GroupByLike[All, Grouped] = CanBeGrouped[All, Grouped]()
}
