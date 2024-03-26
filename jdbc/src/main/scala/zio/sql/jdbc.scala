package zio.sql

import zio._
import zio.stream._
import zio.schema.Schema
import zio.sql.macros.GroupByLike
import zio.sql.update._
import zio.sql.select._
import zio.sql.insert._
import zio.sql.delete._

trait Jdbc extends Sql with JdbcInternalModule with SqlDriverLiveModule with TransactionSyntaxModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def delete(delete: List[Delete[_]]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def update(update: List[Update[_]]): IO[Exception, Int]

    def read[A](read: Read[A]): Stream[Exception, A]

    def insert[A: Schema](insert: Insert[_, A]): IO[Exception, Int]

    def transaction: ZLayer[Any, Exception, SqlTransaction]
  }
  object SqlDriver {

    val live: ZLayer[ConnectionPool, Nothing, SqlDriver] =
      ZLayer(ZIO.serviceWith[ConnectionPool](new SqlDriverLive(_)))
  }

  trait SqlTransaction {

    def delete(delete: Delete[_]): IO[Exception, Int]

    def delete(delete: List[Delete[_]]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def update(update: List[Update[_]]): IO[Exception, Int]

    def read[A](read: Read[A]): Stream[Exception, A]

    def insert[A: Schema](insert: Insert[_, A]): IO[Exception, Int]

  }

  def setParam(param: SqlParameter, jdbcIndex: Int): java.sql.PreparedStatement => Unit

  private[sql] def setParams(rows: List[SqlRow]): java.sql.PreparedStatement => Unit = ps =>
    rows.foreach { row =>
      row.params.zipWithIndex.foreach { case (param, i) =>
        val jdbcIndex = i + 1
        setParam(param, jdbcIndex)(ps)
      }
      ps.addBatch()
    }

  def execute[A](read: Read[A]): ZStream[SqlDriver, Exception, A] =
    ZStream.serviceWithStream(_.read(read))

  def execute[F, A, Source, Subsource, Head, Tail <: SelectionSet[Source]](
    select: Read.Subselect[F, A, Source, Subsource, Head, Tail]
  )(implicit verify: GroupByLike[F, select.GroupByF]): ZStream[SqlDriver, Exception, A] =
    ZStream.serviceWithStream(_.read(select))

  def execute(delete: Delete[_]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.delete(delete))

  def executeBatchDelete(delete: List[Delete[_]]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.delete(delete))

  def execute[A: Schema](insert: Insert[_, A]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.insert(insert))

  def execute(update: Update[_]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.update(update))

  def executeBatchUpdate(update: List[Update[_]]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.update(update))

  val transact: ZLayer[SqlDriver, Exception, SqlTransaction] =
    ZLayer(ZIO.serviceWith[SqlDriver](_.transaction)).flatten

}
