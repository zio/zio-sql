package zio.sql

import scala.language.implicitConversions

trait NewtypesModule {

  type ColumnName = String
  sealed trait TableName {
    def name: String
  }
  object TableName {
    sealed case class Source(name: String) extends TableName
    sealed case class Derived(name: String) extends TableName

    implicit def strToTable(tableName: String) : TableName = TableName.Source(tableName)
  }

  sealed case class FunctionName(name: String)
}
