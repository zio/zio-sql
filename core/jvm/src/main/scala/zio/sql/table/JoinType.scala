package zio.sql.table

sealed trait JoinType

object JoinType {
  case object Inner      extends JoinType
  case object LeftOuter  extends JoinType
  case object RightOuter extends JoinType
  case object FullOuter  extends JoinType
}
