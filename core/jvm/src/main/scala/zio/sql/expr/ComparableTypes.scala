package zio.sql.expr

import scala.annotation.implicitNotFound
import zio.sql.typetag._

@implicitNotFound(
  "You cannot compare values of different types ${A} and ${B}. " +
    "As those are unrelated types, this query would fail at database level."
)
sealed trait ComparableTypes[A, B]

object ComparableTypes extends ComparableTypesLowPriority {
  implicit final def comparableSubtype1[A <: B, B]: ComparableTypes[A, B] = new ComparableTypes[A, B] {}

  implicit final def AWithOptionIsComparable[A]: ComparableTypes[A, Option[A]] = new ComparableTypes[A, Option[A]] {}
  implicit final def optionWithAIsComparable[A]: ComparableTypes[Option[A], A] = new ComparableTypes[Option[A], A] {}

  implicit final def optionAndNone[A]: ComparableTypes[Option[A], None.type] =
    new ComparableTypes[Option[A], None.type] {}
  implicit final def noneAndOption[A]: ComparableTypes[None.type, Option[A]] =
    new ComparableTypes[None.type, Option[A]] {}

  implicit final def optionAndSome[A]: ComparableTypes[Option[A], Expr.Literal[Some[A]]] =
    new ComparableTypes[Option[A], Expr.Literal[Some[A]]] {}
  implicit final def someAndOption[A]: ComparableTypes[Expr.Literal[Some[A]], Option[A]] =
    new ComparableTypes[Expr.Literal[Some[A]], Option[A]] {}

  implicit final def dateIsComprable[A, B](implicit ev1: IsDate[A], ev2: IsDate[B]): ComparableTypes[A, B] =
    new ComparableTypes[A, B] {}

  implicit final def numericIsComparable[A, B](implicit
    ev1: IsNumeric[A],
    ev2: IsNumeric[B]
  ): ComparableTypes[A, B] = new ComparableTypes[A, B] {}
}

sealed trait ComparableTypesLowPriority {
  implicit final def comparableSubtype2[A, B <: A]: ComparableTypes[A, B] = new ComparableTypes[A, B] {}
}
