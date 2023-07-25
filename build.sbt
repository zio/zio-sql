import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/zio-sql/")),
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

val zioVersion                 = "2.0.6"
val zioSchemaVersion           = "0.4.2"
val testcontainersVersion      = "1.17.6"
val testcontainersScalaVersion = "0.40.11"
val logbackVersion             = "1.2.11"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    coreJVM,
    coreJS,
    driver,
    examples,
    jdbc,
    mysql,
    oracle,
    postgres,
    sqlserver,
    jdbc_hikaricp,
    macros,
    docs
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
    resolvers ++= Resolver.sonatypeOssRepos("snapshots")
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val coreJS = core.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val coreJVM = core.jvm.dependsOn(macros)

lazy val macros = project
  .in(file("macros"))
  .settings(stdSettings("zio-sql-macros"))
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value == ScalaDotty) {
        Seq()
      } else
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "dev.zio"       %% "zio"           % zioVersion
        )
    }
  )

lazy val docs = project
  .in(file("zio-sql-docs"))
  .settings(
    moduleName                                 := "zio-sql-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    crossScalaVersions                         := Seq(Scala213, Scala212, ScalaDotty),
    projectName                                := "ZIO SQL",
    mainModuleName                             := (coreJVM / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(),
    docsPublishBranch                          := "master",
    readmeContribution                         := readmeContribution.value +
      """|### TL;DR
         |Prerequisites (installed):
         |
         || Technology | Version |  
         ||------------|---------|
         || sbt        | 1.4.3   |
         || Docker     | 3.1     |
         | 
         |To set up the project follow below steps:
         |1. Fork the repository.
         |2. Setup the upstream (Extended instructions can be followed [here](https://docs.github.com/en/free-pro-team@latest/github/getting-started-with-github/fork-a-repo)).
         |3. Make sure you have installed `sbt` and `Docker`.
         |4. In project directory execute `sbt test`.
         |5. Pick up an issue & you are ready to go!
         |""".stripMargin
  )
  .enablePlugins(WebsitePlugin)

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
    resolvers ++= Resolver.sonatypeOssRepos("snapshots")
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
      "org.postgresql" % "postgresql"                      % "42.4.3"                   % Test,
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

lazy val jdbc_hikaricp = project
  .in(file("jdbc-hikaricp"))
  .settings(stdSettings("zio-sql-jdbc-hickaricp"))
  .settings(buildInfoSettings("zio.sql.jdbc-hickaricp"))
  .settings(
    libraryDependencies ++= Seq(
      "com.zaxxer"         % "HikariCP"                   % "4.0.3", // 5.x doesn't support Java 1.8
      "dev.zio"           %% "zio-test"                   % zioVersion                 % Test,
      "dev.zio"           %% "zio-test-sbt"               % zioVersion                 % Test,
      "org.testcontainers" % "mysql"                      % testcontainersVersion      % Test,
      "mysql"              % "mysql-connector-java"       % "8.0.29"                   % Test,
      "com.dimafeng"      %% "testcontainers-scala-mysql" % testcontainersScalaVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .dependsOn(jdbc)

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
      "mysql"              % "mysql-connector-java"       % "8.0.32"                   % Test,
      "com.dimafeng"      %% "testcontainers-scala-mysql" % testcontainersScalaVersion % Test,
      "ch.qos.logback"     % "logback-classic"            % logbackVersion             % Test
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
      "com.oracle.database.jdbc" % "ojdbc8"                         % "21.7.0.0"                 % Test,
      "com.dimafeng"            %% "testcontainers-scala-oracle-xe" % testcontainersScalaVersion % Test,
      "ch.qos.logback"           % "logback-classic"                % logbackVersion             % Test
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
      "org.postgresql"     % "postgresql"                      % "42.4.3"                   % Compile,
      "com.dimafeng"      %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test,
      "ch.qos.logback"     % "logback-classic"                 % logbackVersion             % Test
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
      "com.dimafeng"           %% "testcontainers-scala-mssqlserver" % testcontainersScalaVersion % Test,
      "ch.qos.logback"          % "logback-classic"                  % logbackVersion             % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
