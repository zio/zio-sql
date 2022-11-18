package zio.sql

import zio.test._
import zio.test.Assertion._
import zio.schema._
import zio.test.ZIOSpecDefault
import zio.schema.DeriveSchema
import java.time.LocalDate

object TestBasicSelect {
  val userSql = new Sql { self =>
    override def renderDelete(delete: self.Delete[_]): String               = ???
    override def renderRead(read: self.Read[_]): String                     = ???
    override def renderUpdate(update: self.Update[_]): String               = ???
    override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???

    case class Users(user_id: String, dob: LocalDate, first_name: String, last_name: String)

    implicit val localDateSchema =
      Schema.primitive[LocalDate](StandardType.LocalDateType)
    implicit val userSchema      = DeriveSchema.gen[Users]

    val userTable = defineTable[Users]

    val (userId, dob, fName, lName) = userTable.columns

    // todo this should compile using column names defined in the table
    val basicSelect = select(fName, lName) from userTable

    // fName and lName already have column names, shouldn't have to do this
    val basicSelectWithAliases = (select(
      (fName as "first_name"),
      (lName as "last_name")
    ) from userTable)
  }
}

object TestBasicSelectSpec extends ZIOSpecDefault {
  import TestBasicSelect.userSql._

  def spec = suite("TestBasicSelectSpec")(
    test("Selecting columns using existing column names") {
      assert(basicSelect)(equalTo(basicSelectWithAliases))
    }
  )
}
