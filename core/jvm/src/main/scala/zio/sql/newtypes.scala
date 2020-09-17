package zio.sql

trait NewtypesModule {

  type ColumnName = String
  type TableName  = String


  sealed case class FunctionName(name: String) extends Renderable {
    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      val _ = builder.append(name)
    }
  }

}