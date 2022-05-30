# ZIO SQL

| Project Stage | CI | Release | Snapshot | Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Stage]][Stage-Page] | ![CI][badge-ci] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] | [![badge-discord]][link-discord] |

## Current status: Non-production release

### Progress report towards 0.1

:heavy_check_mark: - good to go

:white_check_mark: - some more work needed

#### General features:
Feature | Progress
:------------ | :-------------
Type-safe schema | :heavy_check_mark:
Type-safe DSL | :heavy_check_mark:
Running Reads | :heavy_check_mark:
Running Deletes | :heavy_check_mark:
Running Updates | :heavy_check_mark:
Running Inserts | :heavy_check_mark:
Transactions | :white_check_mark:
Connection pool | :white_check_mark:

#### Db-specific features:

Feature | PostgreSQL | SQL Server | Oracle             | MySQL
:------------ | :-------------| :-------------|:-------------------| :-------------
Render Read   | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
Render Delete | :heavy_check_mark: | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | 
Render Update | :heavy_check_mark: |                    | :white_check_mark: | :white_check_mark: |
Render Insert | :heavy_check_mark: |                    |                    | :white_check_mark: |
Functions     | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
Types         | :white_check_mark: |                    |                    | :white_check_mark: |
Operators     |                    |                    |                    |                    |

## What is ZIO SQL?
ZIO SQL lets you write type-safe, type-inferred, and composable SQL queries in ordinary Scala, helping you prevent persistence bugs before they happen, and leverage your IDE to make writing SQL productive, safe, and fun. 

 * **Type-safety**. ZIO SQL queries are type-safe by construction. Most classes of bugs can be detected at compile-time, shortening your feedback loop and helping you use your IDE to write correct queries.
 * **Composable**. All ZIO SQL components are ordinary values, which can be transformed and composed in sensible ways. This uniformity and regularity means you have a lot of power in a small package.
 * **Type-inferred**. ZIO SQL uses maximal variance and lower-kinded types, which means it features very good type inference. You can let Scala figure out the types required for type-safe SQL.
 * **No magic**. ZIO SQL does not need any macros or plug-ins to operate (everything is a value!), and it works across both Scala 2.x and Scala 3. Optionally, Scala schema can be created from database schemas.
 
ZIO SQL can be used as a library for modeling SQL in a type-safe ADT. In addition, ZIO SQL has a JDBC interface, which utilizes the type-safe SQL ADT for interacting with common JDBC databases.

For the JDBC module: 

 - Like Slick, ZIO SQL has an emphasis on type-safe SQL construction using Scala values and methods. However, ZIO SQL utilizes reified lenses, contravariant intersection types, and in-query nullability to improve ergonomics for end-users. Unlike Slick, the intention is to use names resembling SQL instead of trying to mimic the Scala collections.
 - Like Doobie, ZIO SQL is purely functional, but ZIO SQL does compile-time query validation that catches most issues, and has rich ZIO integration, offering improved type-safety compared to monofunctor effects and minimal dependencies (depending only on ZIO).

ZIO SQL does not offer Language Integrated Queries (LINQ) or similar functionality. It is intended only as a data model for representing SQL queries and an accompanying lightweight JDBC-based executor.

[badge-ci]: https://github.com/zio/zio-sql/workflows/CI/badge.svg
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-sql_2.13.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-sql_2.13.svg "Sonatype Snapshots"
[badge-discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-sql_2.13/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-sql_2.13/ "Sonatype Snapshots"
[link-discord]: https://discord.gg/2ccFBr4 "Discord"
[Stage]: https://img.shields.io/badge/Project%20Stage-Development-yellowgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages

## Setup
Prerequisites (installed):

 | Technology   |   Version        |  
 | ------------ | ---------------- |
 | sbt          |    1.4.3         |
 | Docker       |    3.1           |
 
To set up the project follow below steps:
1. Fork the repository.
2. Setup the upstream (Extended instructions can be followed [here](https://docs.github.com/en/free-pro-team@latest/github/getting-started-with-github/fork-a-repo)).
3. Make sure you have installed `sbt` and `Docker`.
4. In project directory execute `sbt test`.
5. Pick up an issue & you are ready to go!

If you want to learn more, please check out:

 - [ZIO SQL Homepage](https://zio.github.io/zio-sql)
 - [ZIO SQL Discord](https://discord.gg/2ccFBr4)
