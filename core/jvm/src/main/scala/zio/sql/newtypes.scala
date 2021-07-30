package zio.sql

trait NewtypesModule {

  type ColumnName = String
  sealed trait TableName 
  object TableName {
    final case class Source(name: String) extends TableName
    case object Derived extends TableName
  }

  sealed case class FunctionName(name: String)

}
