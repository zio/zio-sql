package zio.sql

import java.time.LocalDate
import zio.sql._

object Examples extends App {
  new Sql { self =>
    import self.ColumnSet._

    val userTable = (uuid("user_id") ++ localDate("dob") ++ string("first_name") ++ string("last_name")).table("users")

    val userId :*: dob :*: fName0 :*: lName0 :*: _ = userTable.columns

    //force types for 3rd & subseqent columns for IJ https://youtrack.jetbrains.com/issue/SCL-17599
    val fName: Expr[Features.Source, userTable.TableType, String] = fName0
    val lName: Expr[Features.Source, userTable.TableType, String] = lName0

    val orderTable = (uuid("order_id") ++ uuid("user_id") ++ localDate("order_date")).table("orders")

    val orderId :*: fkUserId :*: orderDate0 :*: _ = orderTable.columns

    //force types for IJ https://youtrack.jetbrains.com/issue/SCL-17599
    val orderDate: Expr[Features.Source, orderTable.TableType, LocalDate] = orderDate0

    //val basicSelect = select { fName ++ lName } from userTable //todo this should compile using column names defined in the table

    val basicSelectWithAliases = select {
      (fName as "first_name") ++ (lName as "last_name")
    } from userTable

    val selectionRefinements =
      (select { (fName as "first_name") ++ (lName as "last_name") }
        from userTable).limit(10).offset(10).orderBy(dob)

    val basicDelete = deleteFrom(userTable).where(fName === "Fred")
  }
}
