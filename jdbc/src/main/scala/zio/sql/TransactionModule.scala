package zio.sql

import java.sql._

import zio.{ Tag => ZTag, _ }
import zio.stream._
import zio.schema.Schema

trait TransactionModule { self: Jdbc =>
  private[sql] sealed case class Txn(connection: Connection, sqlDriverCore: SqlDriverCore)

  sealed case class ZTransaction[-R: ZTag, +E, +A](unwrap: ZIO[(R, Txn), E, A]) { self =>
    def map[B](f: A => B): ZTransaction[R, E, B] =
      ZTransaction(self.unwrap.map(f))

    def flatMap[R1 <: R: ZTag, E1 >: E, B](
      f: A => ZTransaction[R1, E1, B]
    ): ZTransaction[R1, E1, B] =
      ZTransaction(self.unwrap.flatMap(a => f(a).unwrap))

    private[sql] def run(txn: Txn)(implicit
      ev: E <:< Throwable
    ): ZIO[R, Throwable, A] =
      for {
        r <- ZIO.environment[R]
        a <- self.unwrap
               .mapError(ev)
               .provideLayer(ZLayer.succeed((r.get, txn)))
               .absorb
               .tapBoth(
                 _ =>
                   ZIO
                     .attemptBlocking(txn.connection.rollback())
                     .refineToOrDie[Throwable],
                 _ =>
                   ZIO
                     .attemptBlocking(txn.connection.commit())
                     .refineToOrDie[Throwable]
               )
      } yield a

    def zip[R1 <: R: ZTag, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, (A, B)] =
      zipWith[R1, E1, B, (A, B)](tx)((_, _))

    def zipWith[R1 <: R: ZTag, E1 >: E, B, C](
      tx: ZTransaction[R1, E1, B]
    )(f: (A, B) => C): ZTransaction[R1, E1, C] =
      for {
        a <- self
        b <- tx
      } yield f(a, b)

    def *>[R1 <: R: ZTag, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, B] =
      self.flatMap(_ => tx)

    // named alias for *>
    def zipRight[R1 <: R: ZTag, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, B] =
      self *> tx

    def <*[R1 <: R: ZTag, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, A] =
      self.flatMap(a => tx.map(_ => a))

    // named alias for <*
    def zipLeft[R1 <: R: ZTag, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, A] =
      self <* tx

    def catchAllCause[R1 <: R: ZTag, E1 >: E, A1 >: A](
      f: Cause[E1] => ZTransaction[R1, E1, A1]
    ): ZTransaction[R1, E1, A1] =
      ZTransaction(self.unwrap.catchAllCause(cause => f(cause).unwrap))
  }

  object ZTransaction {
    def apply[A](
      read: self.Read[A]
    ): ZTransaction[Any, Exception, zio.stream.Stream[Exception, A]] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        // FIXME: Find a way to NOT load the whole result set into memory at once!!!
        val stream =
          coreDriver.readOn[A](read, connection)

        ZTransaction.fromEffect(stream.runCollect.map(ZStream.fromIterable(_)))
      }

    def apply(update: self.Update[_]): ZTransaction[Any, Exception, Int] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.updateOn(update, connection))
      }

    def batchUpdate(update: List[self.Update[_]]): ZTransaction[Any, Exception, List[Int]] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.updateOnBatch(update, connection))
      }

    def apply[Z: Schema](insert: self.Insert[_, Z]): ZTransaction[Any, Exception, Int] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.insertOn(insert, connection))
      }

    def batchInsert[Z: Schema](insert: List[self.Insert[_, Z]]): ZTransaction[Any, Exception, List[Int]] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.insertOnBatch(insert, connection))
      }

    def apply(delete: self.Delete[_]): ZTransaction[Any, Exception, Int] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.deleteOn(delete, connection))
      }

    def batchDelete(delete: List[self.Delete[_]]): ZTransaction[Any, Exception, List[Int]] =
      txn.flatMap { case Txn(connection, coreDriver) =>
        ZTransaction.fromEffect(coreDriver.deleteOnBatch(delete, connection))
      }

    def succeed[A](a: => A): ZTransaction[Any, Nothing, A] = fromEffect(ZIO.succeed(a))

    def fail[E](e: => E): ZTransaction[Any, E, Nothing] = fromEffect(ZIO.fail(e))

    def halt[E](e: => Cause[E]): ZTransaction[Any, E, Nothing] = fromEffect(ZIO.failCause(e))

    def fromEffect[R: ZTag, E, A](zio: ZIO[R, E, A]): ZTransaction[R, E, A] =
      ZTransaction(for {
        tuple <- ZIO.service[(R, Txn)]
        a     <- zio.provideLayer(ZLayer.succeed((tuple._1)))
      } yield a)

    private val txn: ZTransaction[Any, Nothing, Txn] =
      ZTransaction(ZIO.service[(Any, Txn)].map(_._2))
  }
}
