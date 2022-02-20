package zio.sql

import zio.schema.Schema

trait Sql
    extends SelectModule
    with DeleteModule
    with UpdateModule
    with ExprModule
    with TableModule
    with InsertModule
    with UtilsModule {
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
  def select[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B])(implicit
    i: Features.IsPartiallyAggregated[F]
  ): Selector[F, A, B, i.Unaggregated] =
    Selector[F, A, B, i.Unaggregated](selection)

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

  def insertInto[F, Source, AllColumnIdentities, B <: SelectionSet[Source]](
    table: Table.Source.Aux_[Source, AllColumnIdentities]
  )(
    sources: Selection[F, Source, B]
  ) =
    InsertBuilder[F, Source, AllColumnIdentities, B, sources.ColsRepr](table, sources)

  def renderInsert[A: Schema](insert: self.Insert[_, A]): String
}
