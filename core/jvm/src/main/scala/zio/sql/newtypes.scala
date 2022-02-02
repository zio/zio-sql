package zio.sql

trait NewtypesModule {

  type ColumnName = String
  //TODO we could use zio-prelude new types
  type TableName  = String

  sealed case class FunctionName(name: String)
}
