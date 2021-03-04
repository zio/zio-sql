package zio.sql

import java.sql._
import zio.{ IO, ZIO }
import zio.blocking.Blocking
import zio.stream.{ Stream, ZStream }

trait SqlDriverLiveModule { self: Jdbc =>
  sealed case class SqlDriverLive(blocking: Blocking.Service, pool: ConnectionPool) extends SqlDriver {
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

    def read[A <: SelectionSet[_], Target](
      read: Read[A]
    )(to: read.ResultType => Target): Stream[Exception, Target] =
      ZStream
        .managed(pool.connection)
        .flatMap(readOn(read, _))
        .map(to)

    def readOn[A <: SelectionSet[_]](read: Read[A], conn: Connection): Stream[Exception, read.ResultType] =
      Stream.unwrap {
        blocking.effectBlocking {
          val schema = getColumns(read).zipWithIndex.map { case (value, index) =>
            (value, index + 1)
          } // SQL is 1-based indexing

          val query = renderRead(read)

          val statement = conn.createStatement()

          val _ = statement.execute(query) // TODO: Check boolean return value

          val resultSet = statement.getResultSet()

          ZStream.unfoldM(resultSet) { rs =>
            if (rs.next()) {
              try unsafeExtractRow[read.ResultType](resultSet, schema) match {
                case Left(error)  => ZIO.fail(error)
                case Right(value) => ZIO.succeed(Some((value, rs)))
              } catch {
                case e: SQLException => ZIO.fail(e)
              }
            } else ZIO.succeed(None)
          }

        }.refineToOrDie[Exception]
      }

    override def transact[R, E >: Exception, A](tx: ZTransaction[R, E, A]): ZIO[R, E, A] = {
      def loop(tx: ZTransaction[R, E, Any], conn: Connection): ZIO[R, E, Any] =
        tx match {
          case ZTransaction.Effect(zio)       => zio
          // This does not work because of `org.postgresql.util.PSQLException: This connection has been closed.`
          // case Transaction.Select(read) => ZIO.succeed(readS.executeOn(read, conn))
          // This works and it is eagerly running the Stream
          case ZTransaction.Select(read)      =>
            readOn(read.asInstanceOf[Read[SelectionSet[_]]], conn).runCollect
              .map(a => ZStream.fromIterator(a.iterator))
          case ZTransaction.Update(update)    => updateOn(update, conn)
          case ZTransaction.Delete(delete)    => deleteOn(delete, conn)
          case ZTransaction.FoldCauseM(tx, k) =>
            loop(tx, conn).foldCauseM(
              cause => loop(k.asInstanceOf[ZTransaction.K[R, E, Any, Any]].onHalt(cause), conn),
              success => loop(k.onSuccess(success), conn)
            )
        }

      pool.connection.use(conn =>
        blocking.effectBlocking(conn.setAutoCommit(false)).refineToOrDie[Exception] *>
          loop(tx, conn)
            .tapBoth(
              _ => blocking.effectBlocking(conn.rollback()),
              _ => blocking.effectBlocking(conn.commit())
            )
            .asInstanceOf[ZIO[R, E, A]]
      )
    }
  }
}
