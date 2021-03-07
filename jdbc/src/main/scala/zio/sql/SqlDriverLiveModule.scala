package zio.sql

import java.sql._

import zio._
import zio.blocking.Blocking
import zio.stream.{ Stream, ZStream }

trait SqlDriverLiveModule { self: Jdbc =>
  private[sql] trait SqlDriverCore {
    def deleteOn(delete: Delete[_], conn: Connection): IO[Exception, Int]

    def updateOn(update: Update[_], conn: Connection): IO[Exception, Int]

    def readOn[A](read: Read[A], conn: Connection): Stream[Exception, A]
  }

  sealed case class SqlDriverLive(blocking: Blocking.Service, pool: ConnectionPool)
      extends SqlDriver
      with SqlDriverCore { self =>
    def delete(delete: Delete[_]): IO[Exception, Int] =
      pool.connection.use(deleteOn(delete, _))

    def deleteOn(delete: Delete[_], conn: Connection): IO[Exception, Int] =
      blocking.effectBlocking {
        val query     = renderDelete(delete)
        val statement = conn.createStatement()
        statement.executeUpdate(query)
      }.refineToOrDie[Exception]

    def update(update: Update[_]): IO[Exception, Int] =
      pool.connection.use(updateOn(update, _))

    def updateOn(update: Update[_], conn: Connection): IO[Exception, Int] =
      blocking.effectBlocking {

        val query = renderUpdate(update)

        val statement = conn.createStatement()

        statement.executeUpdate(query)

      }.refineToOrDie[Exception]

    def read[A](
      read: Read[A]
    ): Stream[Exception, A] =
      ZStream
        .managed(pool.connection)
        .flatMap(readOn(read, _))

    override def readOn[A](read: Read[A], conn: Connection): Stream[Exception, A] =
      Stream.unwrap {
        blocking.effectBlocking {
          val schema = getColumns(read).zipWithIndex.map { case (value, index) =>
            (value, index + 1)
          } // SQL is 1-based indexing

          val query = renderRead(read)

          val statement = conn.createStatement()

          val _ = statement.execute(query) // TODO: Check boolean return value

          val resultSet = statement.getResultSet()

          ZStream
            .unfoldM(resultSet) { rs =>
              if (rs.next()) {
                try unsafeExtractRow[read.ResultType](resultSet, schema) match {
                  case Left(error)  => ZIO.fail(error)
                  case Right(value) => ZIO.succeed(Some((value, rs)))
                } catch {
                  case e: SQLException => ZIO.fail(e)
                }
              } else ZIO.succeed(None)
            }
            .map(read.mapper)

        }.refineToOrDie[Exception]
      }

    override def transact[R, A](tx: ZTransaction[R, Exception, A]): ZManaged[R, Exception, A] =
      for {
        connection <- pool.connection
        _          <- blocking.effectBlocking(connection.setAutoCommit(false)).refineToOrDie[Exception].toManaged_
        a          <- tx.run(blocking, Txn(connection, self))
      } yield a
  }
}
