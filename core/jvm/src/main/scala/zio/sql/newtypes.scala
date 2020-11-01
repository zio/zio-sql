package zio.sql

trait NewtypesModule {

  type ColumnName = String
  type TableName  = String

  sealed case class FunctionName(name: String)

}
