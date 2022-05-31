package zio.sql

import zio.{ Tag => ZTag, _ }
import zio.stream._
import zio.schema.Schema

trait Jdbc extends zio.sql.Sql with TransactionModule with JdbcInternalModule with SqlDriverLiveModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def delete(delete: List[Delete[_]]): IO[Exception, List[Int]]

    def update(update: Update[_]): IO[Exception, Int]

    def update(update: List[Update[_]]): IO[Exception, List[Int]]

    def read[A](read: Read[A]): Stream[Exception, A]

    def transact[R, A](tx: ZTransaction[R, Exception, A]): ZIO[R, Throwable, A]

    def insert[A: Schema](insert: Insert[_, A]): IO[Exception, Int]

    def insert[A: Schema](insert: List[Insert[_, A]]): IO[Exception, List[Int]]
  }
  object SqlDriver {

    val live: ZLayer[ConnectionPool, Nothing, SqlDriver] =
      ZLayer(ZIO.serviceWith[ConnectionPool](new SqlDriverLive(_)))
  }

  def execute[R <: SqlDriver: ZTag, A](
    tx: ZTransaction[R, Exception, A]
  ): ZIO[R, Throwable, A] =
    ZIO.serviceWithZIO(_.transact(tx))

  def execute[A](read: Read[A]): ZStream[SqlDriver, Exception, A] =
    ZStream.serviceWithStream(_.read(read))

  def execute(delete: Delete[_]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.delete(delete))

  def executeBatchDelete(delete: List[Delete[_]]): ZIO[SqlDriver, Exception, List[Int]] =
    ZIO.serviceWithZIO(_.delete(delete))

  def execute[A: Schema](insert: Insert[_, A]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.insert(insert))

  def executeBatchInsert[A: Schema](insert: List[Insert[_, A]]): ZIO[SqlDriver, Exception, List[Int]] =
    ZIO.serviceWithZIO(_.insert(insert))

  def execute(update: Update[_]): ZIO[SqlDriver, Exception, Int] =
    ZIO.serviceWithZIO(_.update(update))

  def executeBatchUpdate(update: List[Update[_]]): ZIO[SqlDriver, Exception, List[Int]] =
    ZIO.serviceWithZIO(_.update(update))
}
