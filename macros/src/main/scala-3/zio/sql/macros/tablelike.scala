package zio.sql.macros

sealed trait TableSchema[T]

object TableSchema {

  final case class Compatible[T]() extends TableSchema[T]

  implicit def materializeTableSchema[T]: TableSchema[T] = Compatible[T]()

}
