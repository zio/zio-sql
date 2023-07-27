package zio.sql

import zio.schema.{ Schema, StandardType }
import zio.sql.table._
import zio.sql.update._
import zio.sql.select._
import zio.sql.insert._
import zio.sql.delete._

case class SqlRow(params: List[SqlParameter])
case class SqlParameter(_type: StandardType[_], value: Any)
case class SqlStatement(query: String, rows: List[SqlRow])

trait Sql {

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

  sealed trait Star
  val * : Star = new Star {}

  def select(star: Star): SelectAll = {
    val _ = star
    new SelectAll()
  }

  def select[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SelectBuilder[F, A, B] =
    SelectBuilder[F, A, B](selection)

  def subselect[ParentTable]: SubselectByCommaBuilder[ParentTable] = new SubselectByCommaBuilder[ParentTable]

  def deleteFrom[T <: Table](table: T): Delete[table.TableType] = Delete(table, true)

  def update[A](table: Table.Aux[A]): UpdateBuilder[A] = UpdateBuilder(table)

  val insertInto: InsertByCommaBuilder = InsertByCommaBuilder()

  def renderDelete(delete: Delete[_]): String

  def renderRead(read: Read[_]): String

  def renderUpdate(update: Update[_]): String

  def renderInsert[A: Schema](insert: Insert[_, A]): SqlStatement

  // TODO don't know where to put it now
  implicit def convertOptionToSome[A](implicit op: Schema[Option[A]]): Schema[Some[A]] =
    op.transformOrFail[Some[A]](
      {
        case Some(a) => Right(Some(a))
        case None    => Left("cannot encode Right")
      },
      someA => Right(someA)
    )
  implicit val none: Schema[None.type]                                                 = Schema.singleton(None)
}
