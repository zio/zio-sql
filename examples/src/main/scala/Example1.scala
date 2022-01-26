import zio.sql.Sql
import zio.schema.Schema

object Example1 extends Sql {
  import ColumnSet._

  def renderRead(read: this.Read[_]): String = ???

  def renderDelete(delete: this.Delete[_]): String = ???

  override def renderInsert[A: Schema](insert: Insert[_, A]): String = ???

  def renderUpdate(update: Example1.Update[_]): String = ???

  val columnSet = int("age") ++ string("name")

  val table = columnSet.table("person")

  val table2 = columnSet.table("person2")

  val age :*: name :*: _ = table.columns

  val age2 :*: name2 :*: _ = table2.columns

  import FunctionDef._
  import AggregationDef._

  val queried =
    select(((age + 2) as "age") ++ (name as "name") ++ (Abs(3.0) as "dummy"))
      .from(table)
      .limit(200)
      .offset(1000)
      .orderBy(age.descending)

  val tt =    ((age + 2) as "age")

  val joined =
    select((age as "age") ++ (age2 as "age2"))
      .from(table.join(table2).on(name === name2))

  val aggregated =
    select((Arbitrary(age) as "age") ++ (Count(1) as "count"))
      .from(table)
      .groupBy(age)

  val deleted = deleteFrom(table).where(age === 3)

  val updated =
    update(table)
      .set(age, age + 2)
      .set(name, "foo")
      .where(age > 100)

  val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

  val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns

  val query = select(fkCustomerId ++ Count(orderId))
          .from(orders)
          .groupBy(fkCustomerId, orderDate)

  //TODO remove - just to test group by / having
  def test[F, A, B](expr: Expr[F,A, B])(implicit in: Features.IsPartiallyAggregated[F]) : in.Unaggregated = ???
  def test2[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B])(implicit in: Features.IsPartiallyAggregated[F]) : in.Unaggregated = ???

}
