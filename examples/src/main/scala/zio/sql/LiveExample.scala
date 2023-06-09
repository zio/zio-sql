package example

import java.util.UUID
import java.time.LocalDate
import java.util.Properties

import zio._
import zio.schema.DeriveSchema
import zio.sql.{ ConnectionPool, ConnectionPoolConfig }
import zio.sql.postgresql.PostgresJdbcModule
import zio.sql.table._

/**
 *
 * docker run --name zio-sql-db -p 5432:5432 -e POSTGRES_DB=ziosqltest -e POSTGRES_PASSWORD=12345 -d postgres
 * 
 * psql -h localhost -U postgres -p 5432 -d ziosqltest
 * 
 * create table "customers" (
 *   "id" uuid not null primary key,
 *   "age" integer not null,
 *   "dob" date not null,
 *   "first_name" varchar not null,
 *   "last_name" varchar not null
 * ); 
 *
 */
object LiveExample extends ZIOAppDefault with PostgresJdbcModule {

  import Tables._

  def run = myAppLogic

  val data = Customer(UUID.randomUUID(), 22, LocalDate.ofYearDay(1990, 1), "Ronald", "Russel")

  val stmt = insertInto(customers)(
    userId,
    age,
    dob,
    fName,
    lName
  ).values(data)

  val properties = {
    val p = new Properties()
    p.setProperty("user", "postgres")
    p.setProperty("password", "12345")
    p
  }

  val cpConfig =
    ZLayer(
      ZIO.succeed(
        ConnectionPoolConfig(
          url = "jdbc:postgresql://localhost:5432/ziosqltest?loglevel=2",
          properties = properties,
          autoCommit = true
        )
      )
    )

  val myAppLogic =
    execute(stmt)
      .provide(
        SqlDriver.live,
        ConnectionPool.live,
        cpConfig
      )

  object Tables {

    case class Customer(id: UUID, age: Int, dob: LocalDate, firstName: String, lastName: String)

    implicit val customerSchema = DeriveSchema.gen[Customer]

    val customers = Table.defineTable[Customer]("customers")

    val (userId, age, dob, fName, lName) = customers.columns
  }
}
