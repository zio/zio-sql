package zio.sql.table

import scala.annotation.StaticAnnotation

// TODO add ColumnNameAnnotation
object TableNameAnnotation {
  final case class name(name: String) extends StaticAnnotation
}
