package zio.sql

import zio._
import zio.blocking.Blocking
import zio.stream._
import zio.schema.Schema

trait Jdbc extends zio.sql.Sql with TransactionModule with JdbcInternalModule with SqlDriverLiveModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def read[A](read: Read[A]): Stream[Exception, A]

    def transact[R, A](tx: ZTransaction[R, Exception, A]): ZManaged[R, Exception, A]

    def insertAlt(insert: InsertAlt[_]): IO[Exception, Int]

    def insert[A: zio.schema.Schema](insert: Insert[_, A]): IO[Exception, Int]
  }
  object SqlDriver {
    val live: ZLayer[Blocking with Has[ConnectionPool], Nothing, Has[SqlDriver]] =
      (for {
        blocking <- ZIO.service[Blocking.Service]
        pool     <- ZIO.service[ConnectionPool]
      } yield SqlDriverLive(blocking, pool)).toLayer
  }

  def execute[R <: Has[SqlDriver], A](tx: ZTransaction[R, Exception, A]): ZManaged[R, Exception, A] =
    ZManaged.accessManaged[R](_.get.transact(tx))

  def execute[A](read: Read[A]): ZStream[Has[SqlDriver], Exception, A] =
    ZStream.unwrap(ZIO.access[Has[SqlDriver]](_.get.read(read)))

  def execute(delete: Delete[_]): ZIO[Has[SqlDriver], Exception, Int] =
    ZIO.accessM[Has[SqlDriver]](
      _.get.delete(delete)
    )

  def execute(insert: InsertAlt[_]): ZIO[Has[SqlDriver], Exception, Int] =
    ZIO.accessM[Has[SqlDriver]](
      _.get.insertAlt(insert)
    )

  def execute[A: Schema](insert: Insert[_, A]): ZIO[Has[SqlDriver], Exception, Int] =
    ZIO.accessM[Has[SqlDriver]](
      _.get.insert(insert)
    )
}
