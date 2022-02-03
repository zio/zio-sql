package zio.sql

import zio.{ Tag => ZTag, _ }
import zio.stream._
import zio.schema.Schema

trait Jdbc extends zio.sql.Sql with TransactionModule with JdbcInternalModule with SqlDriverLiveModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def read[A](read: Read[A]): Stream[Exception, A]

    def transact[R, A](tx: ZTransaction[R, Exception, A]): ZManaged[R, Exception, A]

    def insert[A: zio.schema.Schema](insert: Insert[_, A]): IO[Exception, Int]
  }
  object SqlDriver {
    val live: ZLayer[ConnectionPool, Nothing, SqlDriver] =
      (for {
        pool <- ZIO.service[ConnectionPool]
      } yield SqlDriverLive(pool)).toLayer
  }

  def execute[R <: SqlDriver: ZTag: IsNotIntersection, A](
    tx: ZTransaction[R, Exception, A]
  ): ZManaged[R, Exception, A] =
    ZManaged.environmentWithManaged[R](_.get.transact(tx))

  def execute[A](read: Read[A]): ZStream[SqlDriver, Exception, A] =
    ZStream.unwrap(ZIO.environmentWith[SqlDriver](_.get.read(read)))

  def execute(delete: Delete[_]): ZIO[SqlDriver, Exception, Int] =
    ZIO.environmentWithZIO[SqlDriver](
      _.get.delete(delete)
    )

  def execute[A: Schema](insert: Insert[_, A]): ZIO[SqlDriver, Exception, Int] =
    ZIO.environmentWithZIO[SqlDriver](
      _.get.insert(insert)
    )
}
