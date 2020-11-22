package zio.sql

import zio.{ Cause, ZIO }

trait TransactionModule { self: SelectModule with DeleteModule with UpdateModule =>

  import Transaction._

  sealed trait Transaction[-R, +E, +A] { self =>

    def map[B](f: A => B): Transaction[R, E, B] =
      self.flatMap(f andThen Transaction.succeed)

    def flatMap[R1 <: R, E1 >: E, B](f: A => Transaction[R1, E1, B]): Transaction[R1, E1, B] =
      FoldCauseM(self, K[R1, E1, A, B](e => halt(e), f))

    def zip[R1 <: R, E1 >: E, B](tx: Transaction[R1, E1, B]): Transaction[R1, E1, (A, B)] =
      zipWith[R1, E1, B, (A, B)](tx)((_, _))

    def zipWith[R1 <: R, E1 >: E, B, C](tx: Transaction[R1, E1, B])(f: (A, B) => C): Transaction[R1, E1, C] =
      for {
        a <- self
        b <- tx
      } yield f(a, b)

    def *>[R1 <: R, E1 >: E, B](tx: Transaction[R1, E1, B]): Transaction[R1, E1, B] =
      self.flatMap(_ => tx)

    // named alias for *>
    def zipRight[R1 <: R, E1 >: E, B](tx: Transaction[R1, E1, B]): Transaction[R1, E1, B] =
      self *> tx

    def <*[R1 <: R, E1 >: E, B](tx: Transaction[R1, E1, B]): Transaction[R1, E1, A] =
      self.flatMap(a => tx.map(_ => a))

    // named alias for <*
    def zipLeft[R1 <: R, E1 >: E, B](tx: Transaction[R1, E1, B]): Transaction[R1, E1, A] =
      self <* tx

    def catchAllCause[R1 <: R, E1 >: E, A1 >: A](f: Throwable => Transaction[R1, E1, A1]): Transaction[R, E1, A] = ???

  }

  object Transaction {
    case class Effect[R, E, A](zio: ZIO[R, E, A])                                 extends Transaction[R, E, A]
    case class Select[A <: SelectionSet[_]](read: self.Read[A])
        extends Transaction[Any, Exception, zio.stream.Stream[Exception, A]]
    case class Update[A](read: self.Update[A])                                    extends Transaction[Any, Exception, A]
    case class Delete[A](read: self.Delete[_, A])                                 extends Transaction[Any, Exception, A]
    case class FoldCauseM[R, E, A, B](tx: Transaction[R, E, A], k: K[R, E, A, B]) extends Transaction[R, E, B]

    case class K[R, E, A, B](onHalt: Cause[E] => Transaction[R, E, B], onSuccess: A => Transaction[R, E, B])

    def succeed[A](a: A): Transaction[Any, Nothing, A]     = Effect(ZIO.succeed(a))
    def fail[E](e: E): Transaction[Any, E, Nothing]        = Effect(ZIO.fail(e))
    def halt[E](e: Cause[E]): Transaction[Any, E, Nothing] = Effect(ZIO.halt(e))

    def select[A <: SelectionSet[_]](read: self.Read[A]): Transaction[Any, Exception, zio.stream.Stream[Exception, A]] =
      Transaction.Select(read)
    def update[A](update: self.Update[A]): Transaction[Any, Exception, A]                                              =
      Update(update)
    def delete[A](delete: self.Delete[_, A]): Transaction[Any, Exception, A]                                           =
      Delete(delete)

  }
}
