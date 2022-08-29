import zio.sql.Sql
import zio.schema.Schema
import zio.schema.DeriveSchema
import zio.sql.macros.TableSchema

object Example1 extends Sql {

  def renderRead(read: this.Read[_]): String = ???

  def renderDelete(delete: this.Delete[_]): String = ???

  override def renderInsert[A: Schema](insert: Insert[_, A]): String = ???

  def renderUpdate(update: Example1.Update[_]): String = ???

//  case class Address()

  case class Person(name: String, age: Int)

  implicit val personSchema = DeriveSchema.gen[Person]

  def exampleTableLike[T](implicit i: TableSchema[T]): Unit = ()

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Monday extends DayOfWeek
    case object Tuesday extends DayOfWeek
  }

  implicit val dayOfWeekSchema: Schema.Record[DayOfWeek] = ???

  val personTable = defineTable[Person]
  val personTable2 = defineTable[Person]

  //val personTable4 = defineTable[DayOfWeek]

  val (name, age) = personTable.columns
  val (name2, age2) = personTable2.columns

  // InsertBuilder[
  //   Features.Union[
  //     Features.Source[personSchema.field1.label.type,Person],
  //     Features.Source[personSchema.field2.label.type,Person]],
  //     Person,
  //     (personSchema.field1.label.type, personSchema.field2.label.type),
  //     SelectionSet.Cons[Person,String,
  //       SelectionSet.Cons[Person,Int,SelectionSet.Empty]],
  //     (String, (Int, Unit))]
  val xe = insertInto(personTable)(name, age)
  val xeee = insertInto(personTable)(name, age).values(("Jaro", 31))
  //val xeeee = insertInto(personTable)(age, name).values(("Jaro", 31))

  final case class OpPerson(id: java.util.UUID, name: Option[String])

  final case class BigDecWrapper(s: BigDecimal, j: java.math.BigDecimal)
  implicit val bigDecWrapper = DeriveSchema.gen[BigDecWrapper]

  val bigDecTable = defineTable[BigDecWrapper]
  val (jbd, sbd) = bigDecTable.columns

  implicit val opPersonSchema = DeriveSchema.gen[OpPerson]

  val opPersonTable = defineTable[OpPerson]

  val (id, nameOp) = opPersonTable.columns
  

  import FunctionDef._
  import AggregationDef._

  val queried =
    select(((age + 2) as "age"), (name as "name"), (Abs(3.0) as "dummy"))
      .from(personTable)
      .limit(200)
      .offset(1000)
      .orderBy(age.descending)

  val tt = ((age + 2) as "age")

  val joined =
    select((age as "age"), (age2 as "age2"))
      .from(personTable.join(personTable2).on(name === name2))

  val aggregated =
    select((age as "age"), (Count(1) as "count"))
      .from(personTable)
      .groupBy(age)

  val deleted = deleteFrom(personTable).where(age === 3)

  val updated =
    update(personTable)
      .set(age, age + 2)
      .set(name, "foo")
      .where(age > 100)

}
