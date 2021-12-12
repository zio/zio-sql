package zio.sql

import zio.schema.Schema

trait Sql
    extends SelectModule
    with DeleteModule
    with UpdateModule
    with ExprModule
    with TableModule
    with InsertAltModule
    with InsertModule {
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
  def select[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SelectBuilder[F, A, B] =
    SelectBuilder(selection)

  def subselect[ParentTable]: SubselectPartiallyApplied[ParentTable] = new SubselectPartiallyApplied[ParentTable]

  def subselectFrom[ParentTable, F, Source, B <: SelectionSet[Source]](
    parentTable: Table.Aux[ParentTable]
  )(selection: Selection[F, Source, B]) = {
    // parentTable value is here to infer parent table type parameter when doing subqueries
    // e.g. subselectFrom(customers)(orderDate).from(orders).where(customers.id == orders.id))
    val _ = parentTable
    SubselectBuilder[F, Source, B, ParentTable](selection)
  }

  def deleteFrom[T <: Table](table: T): Delete[table.TableType] = Delete(table, true)

  def update[A](table: Table.Aux[A]): UpdateBuilder[A] = UpdateBuilder(table)

  def renderDelete(delete: self.Delete[_]): String

  def renderRead(read: self.Read[_]): String

  def renderUpdate(update: self.Update[_]): String

  def insertAltInto[Source, N <: ColumnCount, AllColumnIdentities](
    table: Table.Source.AuxN[Source, AllColumnIdentities, N]
  ): InsertAltBuilder[Source, N, AllColumnIdentities] =
    InsertAltBuilder(table)

  def renderInsertAlt(insert: self.InsertAlt[_]): String

  def insertInto[F, Source, N <: ColumnCount, AllColumnIdentities, B <: SelectionSet.Aux[Source, ColsRepr], ColsRepr](
    table: Table.Source.AuxN[Source, AllColumnIdentities, N]
  )(
    sources: Selection.Aux[F, Source, B, ColsRepr]
    //TODO verify if I really need to check Size
  )(implicit ev2: N =:= sources.Size) =
    InsertBuilder(table, sources)

  def renderInsert[A: Schema](insert: self.Insert[_, A]): String
}
