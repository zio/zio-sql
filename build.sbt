import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.github.io/zio-sql/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
      Developer("jdegoes", "John De Goes", "john@degoes.net", url("http://degoes.net"))
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo       := Some(
      ScmInfo(url("https://github.com/zio/zio-sql/"), "scm:git:git@github.com:zio/zio-sql.git")
    )
  )
)

addCommandAlias("fmtOnce", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fmt", "fmtOnce;fmtOnce")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

val zioVersion                 = "2.0.0-RC6"
val zioSchemaVersion           = "0.1.9"
val testcontainersVersion      = "1.17.2"
val testcontainersScalaVersion = "0.40.7"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
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
    sqlserver
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(stdSettings("zio-sql"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.sql"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"                   % zioVersion,
      "dev.zio" %% "zio-streams"           % zioVersion,
      "dev.zio" %% "zio-schema"            % zioSchemaVersion,
      "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion,
      "dev.zio" %% "zio-test"              % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt"          % zioVersion % Test
    ),
    dependencyOverrides += "dev.zio" %% "zio" % zioVersion,
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val coreJS = core.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val coreJVM = core.jvm

lazy val docs = project
  .in(file("zio-sql-docs"))
  .settings(
    publish / skip := true,
    moduleName     := "zio-sql-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings"
  )
  .dependsOn(postgres)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)

lazy val examples = project
  .in(file("examples"))
  .settings(stdSettings("examples"))
  .settings(
    publish / skip := true,
    moduleName     := "examples"
  )
  .dependsOn(postgres)

lazy val driver = project
  .in(file("driver"))
  .settings(stdSettings("zio-sql-driver"))
  .settings(buildInfoSettings("zio.sql.driver"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"                   % zioVersion,
      "dev.zio" %% "zio-schema"            % zioSchemaVersion,
      "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion,
      "dev.zio" %% "zio-test"              % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt"          % zioVersion % Test
    ),
    dependencyOverrides += "dev.zio" %% "zio" % zioVersion,
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val jdbc = project
  .in(file("jdbc"))
  .settings(stdSettings("zio-sql-jdbc"))
  .settings(buildInfoSettings("zio.sql.jdbc"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-test"                        % zioVersion                 % Test,
      "dev.zio"       %% "zio-test-sbt"                    % zioVersion                 % Test,
      "org.postgresql" % "postgresql"                      % "42.3.6"                   % Test,
      "com.dimafeng"  %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test
    )
  )
  .settings(
    Seq(
      Compile / doc / scalacOptions ++= Seq(
        "-no-link-warnings" // Suppresses problems with Scaladoc links
      )
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(core.jvm)

lazy val mysql = project
  .in(file("mysql"))
  .dependsOn(jdbc % "compile->compile;test->test")
  .dependsOn(driver)
  .settings(stdSettings("zio-sql-mysql"))
  .settings(buildInfoSettings("zio.sql.mysql"))
  .settings(
    libraryDependencies ++= Seq(
      "org.testcontainers" % "testcontainers"             % testcontainersVersion      % Test,
      "org.testcontainers" % "database-commons"           % testcontainersVersion      % Test,
      "org.testcontainers" % "jdbc"                       % testcontainersVersion      % Test,
      "org.testcontainers" % "mysql"                      % testcontainersVersion      % Test,
      "mysql"              % "mysql-connector-java"       % "8.0.29"                   % Test,
      "com.dimafeng"      %% "testcontainers-scala-mysql" % testcontainersScalaVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val oracle = project
  .in(file("oracle"))
  .dependsOn(jdbc % "compile->compile;test->test")
  .dependsOn(driver)
  .settings(stdSettings("zio-sql-oracle"))
  .settings(buildInfoSettings("zio.sql.oracle"))
  .settings(
    libraryDependencies ++= Seq(
      "org.testcontainers"       % "testcontainers"                 % testcontainersVersion      % Test,
      "org.testcontainers"       % "database-commons"               % testcontainersVersion      % Test,
      "org.testcontainers"       % "oracle-xe"                      % testcontainersVersion      % Test,
      "org.testcontainers"       % "jdbc"                           % testcontainersVersion      % Test,
      "com.oracle.database.jdbc" % "ojdbc8"                         % "21.5.0.0"                 % Test,
      "com.dimafeng"            %% "testcontainers-scala-oracle-xe" % testcontainersScalaVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val postgres = project
  .in(file("postgres"))
  .dependsOn(jdbc % "compile->compile;test->test")
  .dependsOn(driver)
  .settings(stdSettings("zio-sql-postgres"))
  .settings(buildInfoSettings("zio.sql.postgres"))
  .settings(
    libraryDependencies ++= Seq(
      "org.testcontainers" % "testcontainers"                  % testcontainersVersion      % Test,
      "org.testcontainers" % "database-commons"                % testcontainersVersion      % Test,
      "org.testcontainers" % "postgresql"                      % testcontainersVersion      % Test,
      "org.testcontainers" % "jdbc"                            % testcontainersVersion      % Test,
      "org.postgresql"     % "postgresql"                      % "42.3.6"                   % Compile,
      "com.dimafeng"      %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val sqlserver = project
  .in(file("sqlserver"))
  .dependsOn(jdbc % "compile->compile;test->test")
  .dependsOn(driver)
  .settings(stdSettings("zio-sql-sqlserver"))
  .settings(buildInfoSettings("zio.sql.sqlserver"))
  .settings(
    libraryDependencies ++= Seq(
      "org.testcontainers"      % "testcontainers"                   % testcontainersVersion      % Test,
      "org.testcontainers"      % "database-commons"                 % testcontainersVersion      % Test,
      "org.testcontainers"      % "mssqlserver"                      % testcontainersVersion      % Test,
      "org.testcontainers"      % "jdbc"                             % testcontainersVersion      % Test,
      "com.microsoft.sqlserver" % "mssql-jdbc"                       % "9.4.0.jre8"               % Test,
      "com.dimafeng"           %% "testcontainers-scala-mssqlserver" % testcontainersScalaVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
