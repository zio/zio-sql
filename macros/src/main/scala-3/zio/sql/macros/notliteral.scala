package zio.sql.macros 

sealed trait IsNotLiteral[F]

object IsNotLiteral {

    final case class FIsNotLiteral[F]() extends IsNotLiteral[F]

    implicit def materializeIsNotLiteral[F]: IsNotLiteral[F] = {
        FIsNotLiteral[F]()
    }
}