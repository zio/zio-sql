package zio.sql

import zio.{Cause, ZIO}

trait TransactionModule { self : SelectModule with DeleteModule with UpdateModule =>

  import Transaction._

  sealed trait Transaction[-R, +A] { self =>

    def map[B](f: A => B): Transaction[R, B] =
      self.flatMap(f andThen Transaction.succeed)

    def flatMap[B, R1 <: R](f: A => Transaction[R1, B]): Transaction[R1, B] =
      FoldCauseM(self, K[R1, Exception, A, B](e => fail(new Exception(e.toString)), f))

    def zip[B, R1 <: R](tx: Transaction[R1, B]): Transaction[R1, (A, B)] =
      zipWith[R1, B, (A, B)](tx)((_, _))

    def zipWith[R1 <: R, B, C](tx: Transaction[R1, B])(f: (A, B) => C): Transaction[R1, C] =
      for {
        a <- self
        b <- tx
      } yield f(a, b)

    def *>[B, R1 <: R](tx: Transaction[R1, B]): Transaction[R1, B] =
      self.flatMap(_ => tx)

    // named alias for *>
    def zipRight[B, R1 <: R](tx: Transaction[R1, B]): Transaction[R1, B] =
      self *> tx

    def <*[B, R1 <: R](tx: Transaction[R1, B]): Transaction[R1, A] =
      self.flatMap(a => tx.map(_ => a))

    // named alias for <*
    def zipLeft[B, R1 <: R](tx: Transaction[R1, B]): Transaction[R1, A] =
      self <* tx

    def catchAllCause[A1 >: A, R1 <: R](f: Throwable => Transaction[R1, A1]): Transaction[R, A] = ???

  }

  object Transaction {
    case class Effect[R, A](zio: ZIO[R, Throwable, A]) extends Transaction[R, A]
    case class Select[A <: SelectionSet[_]](read: Read[A]) extends Transaction[Any, zio.stream.Stream[Exception, A]]
    case class Update(read: self.Update[_]) extends Transaction[Any, Int]
    case class Delete(read: self.Delete[_, _]) extends Transaction[Any, Int]
    // catchAll and flatMap
    case class FoldCauseM[R, E, A, B](tx: Transaction[R, A], k: K[R, E, A, B]) extends Transaction[R, B]

//    final case class MakeSavePoint[R, A, B](tx: Transaction[R, A]) extends Transaction[R, B]
//    final case class RollbackSavePoint[R, A, B](tx: Transaction[R, A]) extends Transaction[R, B]

    case class K[R, E, A, B](onHalt: Cause[E] => Transaction[R, B], onSuccess: A => Transaction[R, B])

    def succeed[A](a: A): Transaction[Any, A] = Effect(ZIO.succeed(a))
    def fail[E <: Throwable](e: E): Transaction[Any, Nothing] = Effect(ZIO.fail(e))

//    def savepoint[R, A](sp: Transaction[Any, Nothing] => Transaction[R, A]): Transaction[R, A] = ???


    def select[A <: SelectionSet[_]](read: Read[A]): Transaction[Any, A] = ???
    def update(read: self.Update[_]): Transaction[Any, Long] = ???
    def delete(read: self.Delete[_, _]): Transaction[Any, Long] = ???

  }



//  val query: Read[String] = ???
//  val del: self.Delete[_, _] = ???

//  import Transaction._

//  savepoint(rollback => for {
//   s <- select(query)
//   _ <- delete(del).zipRight( rollback )
//  } yield s)


//  (for { // SP1
//   s <- select(query)
//   s2 <- select(query)
//   // SP2
//   _ <- delete(del).catchAll(_ => succeed(1))
//   _ <- delete(del)
////   _ <- Transaction.when(_ > 5)(fail(???))
//  } yield s).catchAll(_ => fail("Asdfasd"))

}
