package zio.sql.select

import zio.sql.expr.Expr
import scala.language.implicitConversions

sealed trait Ordering[+A] {
  val value: A
}

object Ordering {
  final case class Asc[A](value: A)  extends Ordering[A]
  final case class Desc[A](value: A) extends Ordering[A]

  implicit def exprToOrdering[F, A, B](expr: Expr[F, A, B]): Ordering[Expr[F, A, B]] =
    Asc(expr)
}
