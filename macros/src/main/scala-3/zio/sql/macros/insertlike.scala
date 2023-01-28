package zio.sql.macros

sealed trait InsertLike[F, ColsRepr, AllColumnIdentities, Z]

object InsertLike {

  final case class CanBeInserted[F, ColsRepr, AllColumnIdentities, Z]()
      extends InsertLike[F, ColsRepr, AllColumnIdentities, Z]

  implicit def createInsertLike[
    F,
    ColsRepr,
    AllColumnIdentities,
    Z
  ]: InsertLike[
    F,
    ColsRepr,
    AllColumnIdentities,
    Z,
  ] = CanBeInserted[F, ColsRepr, AllColumnIdentities, Z]()
}
