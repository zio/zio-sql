package zio.sql

import zio.schema.Schema

trait Sql
    extends SelectModule
    with GroupByUtilsModule
    with DeleteModule
    with UpdateModule
    with ExprModule
    with TableModule
    with InsertModule
    with UtilsModule
    with SelectUtilsModule
    with InsertUtilsModule {
  self =>

  /*
   * (SELECT *, "foo", table.a + table.b AS sum... FROM table WHERE cond) UNION (SELECT ... FROM table)
   *   UNION ('1', '2', '3')
   *   ORDER BY table.a ASC, foo, sum DESC
   *   LIMIT 200
   *   OFFSET 100
   * UPDATE table SET ...
   * INSERT ... INTO table
   * DELETE ... FROM table
   *
   * SELECT ARBITRARY(age), COUNT(*) FROM person GROUP BY age
   */
  val select: SelectByCommaBuilder = SelectByCommaBuilder()
  
  def subselect[ParentTable]: SubselectPartiallyApplied[ParentTable] = new SubselectPartiallyApplied[ParentTable]

  def deleteFrom[T <: Table](table: T): Delete[table.TableType] = Delete(table, true)

  def update[A](table: Table.Aux[A]): UpdateBuilder[A] = UpdateBuilder(table)

  def insertInto[Source, AllColumnIdentities](
    table: Table.Source.Aux_[Source, AllColumnIdentities]
  ): InsertIntoBuilder[Source, AllColumnIdentities] = InsertIntoBuilder(table)

  def renderDelete(delete: self.Delete[_]): String

  def renderRead(read: self.Read[_]): String

  def renderUpdate(update: self.Update[_]): String

  def renderInsert[A: Schema](insert: self.Insert[_, A]): String
}
