import BuildHelper._
import InfrastructureHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("jdegoes", "John De Goes", "john@degoes.net", url("http://degoes.net"))
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val startPostgres = taskKey[Unit]("Start up Postgres")
startPostgres := startService(Database.Postgres, streams.value)

lazy val stopPostgres = taskKey[Unit]("Shut down Postgres")
stopPostgres := stopService(Database.Postgres, streams.value)

lazy val startMySQL = taskKey[Unit]("Start up MySQL")
startMySQL := startService(Database.MySQL, streams.value)

lazy val stopMySQL = taskKey[Unit]("Shut down MySQL")
stopMySQL := stopService(Database.MySQL, streams.value)

lazy val startMsSQL = taskKey[Unit]("Start up Microsoft SQL Server")
startMsSQL := startService(Database.MSSQL, streams.value)

lazy val stopMsSQL = taskKey[Unit]("Shut down Microsoft SQL Server")
stopMsSQL := stopService(Database.MSSQL, streams.value)

lazy val startOracle = taskKey[Unit]("Start up Oracle")
startOracle := startService(Database.Oracle, streams.value)

lazy val stopOracle = taskKey[Unit]("Shut down Oracle")
stopOracle := stopService(Database.Oracle, streams.value)

lazy val root = project
  .in(file("."))
  .settings(stdSettings("zio-sql"))
