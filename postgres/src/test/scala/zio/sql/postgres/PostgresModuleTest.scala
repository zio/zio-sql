package zio.sql.postgres

import java.time.LocalDate
import java.util.UUID

import zio.blocking.Blocking
import zio.sql.ShopSchema
import zio.sql.postgresql.PostgresModule
import zio.test._
import zio.test.Assertion._

object PostgresModuleTest extends DefaultRunnableSpec with PostgresIntegrationTestBase with PostgresModule with ShopSchema {
  
  val executorLayer = ((Blocking.live ++ connectionPoolLayer) >>> ReadExecutor.live)
  val testEnv = zio.test.environment.testEnvironment ++ executorLayer//++ postgresLayer

  import this.AggregationDef._
  import this.Users._
  import this.Orders._

  case class User(id: UUID, fname: String, lname: String, dateOfBirth: LocalDate)

  val spec = suite("Postgres module")(
    testM("Select from one table") {
      val query = select { userId ++ fName ++ lName ++ dob } from users

      val expected = 
        Seq(
          User(UUID.fromString("60b01fc9-c902-4468-8d49-3c0f989def37"), "Ronald", "Russell", LocalDate.parse("1983-01-05")),
          User(UUID.fromString("f76c9ace-be07-4bf3-bd4c-4a9c62882e64"), "Terrence", "Noel", LocalDate.parse("1999-11-02")),
          User(UUID.fromString("784426a5-b90a-4759-afbb-571b7a0ba35e"), "Mila", "Paterso", LocalDate.parse("1990-11-16")),
          User(UUID.fromString("df8215a2-d5fd-4c6c-9984-801a1b3a2a0b"), "Alana", "Murray", LocalDate.parse("1995-11-12")),
          User(UUID.fromString("636ae137-5b1a-4c8c-b11f-c47c624d9cdc"), "Jose", "Wiggins", LocalDate.parse("1987-03-23"))
        )

      val result = new ExecuteBuilder(query).to[UUID, String, String, LocalDate, User] { case row =>
        User(row._1, row._2, row._3, row._4)
      }.provideCustomLayer(executorLayer)

      for {
        r <- result.runCollect
      } yield assert(r)(hasSameElementsDistinct(expected))
    },
    testM("Can count rows") {
      val query = select { Count(userId) } from users

      val expected = 5L

      val result = new ExecuteBuilder(query).to[Long, Long](identity).provideCustomLayer(executorLayer)

      for {
        r <- result.runCollect
      } yield assert(r.head)(equalTo(expected))
    },
    testM("Can select from joined tables") {
      val query = select { fName ++ lName ++ orderDate } from (users leftOuter orders).on(fkUserId === userId)

      case class Row(firstName: String, lastName: String, orderDate: LocalDate)

      val expected = Seq(Row("Ronald", "Russell", LocalDate.parse("2019-03-25")))

      val result = new ExecuteBuilder(query).to[String, String, LocalDate, Row] { case row =>
        Row(row._1, row._2, row._3)
      }.provideCustomLayer(executorLayer)

      for {
        r <- result.runCollect
      } yield assert(r)(equalTo(expected))
    }
  )

}