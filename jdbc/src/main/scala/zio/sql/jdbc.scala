package zio.sql

import zio._
import zio.stream._
import zio.schema.Schema

trait Jdbc extends zio.sql.Sql with JdbcInternalModule with SqlDriverLiveModule with ExprSyntaxModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def delete(delete: List[Delete[_]]): IO[Exception, List[Int]]

    def update(update: Update[_]): IO[Exception, Int]

    def update(update: List[Update[_]]): IO[Exception, List[Int]]

    def read[A](read: Read[A]): Stream[Exception, A]

    def insert[A: Schema](insert: Insert[_, A]): IO[Exception, Int]

    def insert[A: Schema](insert: List[Insert[_, A]]): IO[Exception, List[Int]]

    def transaction: ZLayer[Any, Exception, SqlTransaction]
  }
  object SqlDriver {

    val live: ZLayer[ConnectionPool, Nothing, SqlDriver] =
      ZLayer(ZIO.serviceWith[ConnectionPool](new SqlDriverLive(_)))
  }

  trait SqlTransaction {

    def delete(delete: Delete[_]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def read[A](read: Read[A]): Stream[Exception, A]

    def insert[A: Schema](insert: Insert[_, A]): IO[Exception, Int]

  }

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

  val transact: ZLayer[SqlDriver, Exception, SqlTransaction] =
    ZLayer(ZIO.serviceWith[SqlDriver](_.transaction)).flatten
}
