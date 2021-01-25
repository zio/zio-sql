package zio.sql

import java.util.UUID

trait NewtypesModule {

  type ColumnName = String
  type TableId    = UUID
  type TableName  = String
  type TableAlias = String

  sealed case class FunctionName(name: String)

}
