package zio.sql

import zio.{ Has, IO, ZIO, ZLayer }
import zio.blocking.Blocking
import zio.stream.Stream

trait Jdbc
    extends zio.sql.Sql
    with TransactionModule
    with ExecuteBuilderModule
    with JdbcInternalModule
    with SqlDriverLiveModule {
  trait SqlDriver  {
    def delete(delete: Delete[_]): IO[Exception, Int]

    def update(update: Update[_]): IO[Exception, Int]

    def read[A <: SelectionSet[_], Target](read: Read[A])(to: read.ResultType => Target): Stream[Exception, Target]

    def transact[R, A](tx: ZTransaction[R, Exception, A]): ZIO[R, Exception, A]
  }
  object SqlDriver {
    val live: ZLayer[Blocking with Has[ConnectionPool], Nothing, Has[SqlDriver]] =
      (for {
        blocking <- ZIO.service[Blocking.Service]
        pool     <- ZIO.service[ConnectionPool]
      } yield SqlDriverLive(blocking, pool)).toLayer
  }

  def execute[R <: Has[SqlDriver], A](tx: ZTransaction[R, Exception, A]): ZIO[R, Exception, A] =
    ZIO.accessM[R](_.get.transact(tx))

  def execute[A <: SelectionSet[_]](read: Read[A]): ExecuteBuilder[A, read.ResultType] =
    new ExecuteBuilder(read)

  def execute(delete: Delete[_]): ZIO[Has[SqlDriver], Exception, Int] =
    ZIO.accessM[Has[SqlDriver]](
      _.get.delete(delete)
    )
}
