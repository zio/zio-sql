package zio.sql.postgresql

import zio.Cause
import zio.test.Assertion._
import zio.test._

object SelectSpec extends PostgresRunnableSpec with DbSchema {

  override def specLayered = suite("Postgres module select")(
    test("Select null int value as None Option") {
      val movieColumns = MoviesSchema.id ++ MoviesSchema.rating
      val selectStmt   = select(movieColumns).from(MoviesSchema.movies).where(MoviesSchema.id === 1)
      val selectResult = execute(selectStmt.to((MoviesSchema.Movies.apply _).tupled)).runCollect

      val insertAssertion =
        for {
          movies <- selectResult
        } yield assert(movies.toList)(
          equalTo(
            List(
              MoviesSchema.Movies(
                id = 1,
                rating = None
              )
            )
          )
        )
      insertAssertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )

}
