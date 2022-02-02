package zio.sql

import zio.test._
import zio.test.Assertion._
import zio.schema.Schema

object TestBasicSelect {
  val userSql = new Sql { self =>
    import self.ColumnSet._

    override def renderDelete(delete: self.Delete[_]): String               = ???
    override def renderRead(read: self.Read[_]): String                     = ???
    override def renderUpdate(update: self.Update[_]): String               = ???
    override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???

    val userTable =
      (string("user_id") ++ localDate("dob") ++ string("first_name") ++ string("last_name")).table("users")

    val userId :*: dob :*: fName :*: lName :*: _ = userTable.columns

    //todo this should compile using column names defined in the table
    val basicSelect = select(fName ++ lName) from userTable

    // fName and lName already have column names, shouldn't have to do this
    val basicSelectWithAliases = (select {
      (fName as "first_name") ++ (lName as "last_name")
    } from userTable)
  }
}

object TestBasicSelectSpec extends DefaultRunnableSpec {
  import TestBasicSelect.userSql._

  def spec = suite("TestBasicSelectSpec")(
    test("Selecting columns using existing column names") {
      assert(basicSelect)(equalTo(basicSelectWithAliases))
    }
  )
}
