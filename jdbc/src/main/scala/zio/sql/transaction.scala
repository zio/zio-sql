package zio.sql

import zio.{ Cause, ZIO }

trait TransactionModule { self: SelectModule with DeleteModule with UpdateModule =>

  type Transaction[+E, +A] = ZTransaction[Any, E, A]

  sealed trait ZTransaction[-R, +E, +A] { self =>
    import ZTransaction._

    def map[B](f: A => B): ZTransaction[R, E, B] =
      self.flatMap(f andThen ZTransaction.succeed)

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZTransaction[R1, E1, B]): ZTransaction[R1, E1, B] =
      FoldCauseM(self, K[R1, E1, A, B](e => halt(e), f))

    def zip[R1 <: R, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, (A, B)] =
      zipWith[R1, E1, B, (A, B)](tx)((_, _))

    def zipWith[R1 <: R, E1 >: E, B, C](tx: ZTransaction[R1, E1, B])(f: (A, B) => C): ZTransaction[R1, E1, C] =
      for {
        a <- self
        b <- tx
      } yield f(a, b)

    def *>[R1 <: R, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, B] =
      self.flatMap(_ => tx)

    // named alias for *>
    def zipRight[R1 <: R, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, B] =
      self *> tx

    def <*[R1 <: R, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, A] =
      self.flatMap(a => tx.map(_ => a))

    // named alias for <*
    def zipLeft[R1 <: R, E1 >: E, B](tx: ZTransaction[R1, E1, B]): ZTransaction[R1, E1, A] =
      self <* tx

    def catchAllCause[R1 <: R, E1 >: E, A1 >: A](f: E1 => ZTransaction[R1, E1, A1]): ZTransaction[R1, E1, A1] =
      FoldCauseM(
        self,
        K(
          (cause: Cause[E1]) =>
            cause match {
              case Cause.Fail(e) => f(e)
              case cause         => halt(cause)
            },
          succeed[A1]
        )
      )

  }

  object ZTransaction {
    case class Effect[R, E, A](zio: ZIO[R, E, A])                                  extends ZTransaction[R, E, A]
    case class Select[A <: SelectionSet[_]](read: self.Read[A])
        extends ZTransaction[Any, Exception, zio.stream.Stream[Exception, A]]
    case class Update(read: self.Update[_])                                        extends ZTransaction[Any, Exception, Int]
    case class Delete(read: self.Delete[_])                                        extends ZTransaction[Any, Exception, Int]
    case class FoldCauseM[R, E, A, B](tx: ZTransaction[R, E, A], k: K[R, E, A, B]) extends ZTransaction[R, E, B]

    case class K[R, E, A, B](onHalt: Cause[E] => ZTransaction[R, E, B], onSuccess: A => ZTransaction[R, E, B])

    def succeed[A](a: A): ZTransaction[Any, Nothing, A]     = Effect(ZIO.succeed(a))
    def fail[E](e: E): ZTransaction[Any, E, Nothing]        = Effect(ZIO.fail(e))
    def halt[E](e: Cause[E]): ZTransaction[Any, E, Nothing] = Effect(ZIO.halt(e))

    def select[A <: SelectionSet[_]](
      read: self.Read[A]
    ): ZTransaction[Any, Exception, zio.stream.Stream[Exception, A]]      =
      ZTransaction.Select(read)
    def update(update: self.Update[_]): ZTransaction[Any, Exception, Int] =
      Update(update)
    def delete(delete: self.Delete[_]): ZTransaction[Any, Exception, Int] =
      Delete(delete)

  }
}
