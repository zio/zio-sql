import BuildHelper._
import InfrastructureHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-sql/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("jdegoes", "John De Goes", "john@degoes.net", url("http://degoes.net"))
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-sql/"), "scm:git:git@github.com:zio/zio-sql.git")
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

val zioVersion = "1.0.3"

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
  .settings(
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    coreJVM,
    coreJS,
    docs,
    driver,
    examples,
    jdbc,
    mysql,
    oracle,
    postgres,
    sqlserver,
    test
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(stdSettings("zio-sql"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.sql"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val coreJS = core.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val coreJVM = core.jvm
  .settings(dottySettings)

lazy val docs = project
  .in(file("zio-sql-docs"))
  .settings(
    skip.in(publish) := true,
    moduleName := "zio-sql-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    //unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(coreJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val examples = project
  .in(file("examples"))
  .settings(
    skip in publish := true,
    moduleName := "examples"
  )
  .dependsOn(core.jvm)
  .dependsOn(jdbc)

lazy val driver = project
  .in(file("driver"))
  .settings(stdSettings("zio-sql-driver"))
  .settings(buildInfoSettings("zio.sql.driver"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val jdbc = project
  .in(file("jdbc"))
  .settings(stdSettings("zio-sql-jdbc"))
  .settings(buildInfoSettings("zio.sql.jdbc"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(core.jvm)

lazy val mysql = project
  .in(file("mysql"))
  .settings(stdSettings("zio-sql-mysql"))
  .settings(buildInfoSettings("zio.sql.mysql"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val oracle = project
  .in(file("oracle"))
  .settings(stdSettings("zio-sql-oracle"))
  .settings(buildInfoSettings("zio.sql.oracle"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val postgres = project
  .in(file("postgres"))
  .settings(stdSettings("zio-sql-postgres"))
  .settings(buildInfoSettings("zio.sql.postgres"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val sqlserver = project
  .in(file("sqlserver"))
  .settings(stdSettings("zio-sql-sqlserver"))
  .settings(buildInfoSettings("zio.sql.sqlserver"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val test = project
  .in(file("test"))
  .settings(stdSettings("zio-sql-test"))
  .settings(buildInfoSettings("zio.sql.test"))
  .settings(
    skip in publish := true,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
