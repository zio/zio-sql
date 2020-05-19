# ZIO SQL

ZIO SQL lets you write type-safe, type-inferred, and composable SQL queries in ordinary Scala, helping you prevent persistence bugs before they happen, and leverage your IDE to make writing SQL productive, safe, and fun. 

 * **Type-safety**. ZIO SQL queries are type-safe by construction. Most classes of bugs can be detected at compile-time, shortening your feedback loop and helping you use your IDE to write correct queries.
 * **Composable**. All ZIO SQL components are ordinary values, which can be transformed and composed in sensible ways. This uniformity and regularity means you have a lot of power in a small package.
 * **Type-inferred**. ZIO SQL uses maximal variance and lower-kinded types, which means it features very good type inference. You can let Scala figure out the types required for type-safe SQL.
 * **No magic**. ZIO SQL does not need any macros or plug-ins to operate (everything is a value!), and it works across both Scala 2.x and Scala 3. Optionally, Scala schema can be created from database schemas.
 
ZIO SQL can be used as a library for modeling SQL in a type-safe ADT. In addition, ZIO SQL has a JDBC interface, which utilizes the type-safe SQL ADT for interacting with JDBC databases.

For the JDBC module: 

 - Like Slick, ZIO SQL has an emphasis on type-safe SQL construction using Scala values and methods. However, ZIO SQL utilizes reified lenses, intersection types, and in-query nullability to improve ergonomics. 
 - Like Doobie, ZIO SQL is purely functional, but ZIO SQL does compile-time query validation that catches most issues, and has rich ZIO integration, offering improved type-safety and minimal dependencies.

ZIO SQL does not offer Language Integrated Queries (LINQ) or similar functionality. It is intended only as a model for representing SQL queries and lightweight JDBC executor.

If you want to learn more, please check out:

[ZIO SQL Homepage](https://zio.github.io/zio-sql)
[ZIO SQL Discord](https://discord.gg/2ccFBr4)
