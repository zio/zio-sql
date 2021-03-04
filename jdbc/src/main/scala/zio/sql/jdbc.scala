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

    def transact[R, E >: Exception, A](tx: ZTransaction[R, E, A]): ZIO[R, E, A]
  }
  object SqlDriver {
    val live: ZLayer[Blocking with Has[ConnectionPool], Nothing, Has[SqlDriver]] =
      (for {
        blocking <- ZIO.service[Blocking.Service]
        pool     <- ZIO.service[ConnectionPool]
      } yield SqlDriverLive(blocking, pool)).toLayer
  }

  def execute[R, E >: Exception, A](tx: ZTransaction[R, E, A]): ZIO[R with Has[SqlDriver], E, A] =
    ZIO.accessM[R with Has[SqlDriver]](_.get.transact(tx))

  def execute[A <: SelectionSet[_]](read: Read[A]): ExecuteBuilder[A, read.ResultType] =
    new ExecuteBuilder(read)

  def execute(delete: Delete[_]): ZIO[Has[SqlDriver], Exception, Int] =
    ZIO.accessM[Has[SqlDriver]](
      _.get.delete(delete)
    )

}
