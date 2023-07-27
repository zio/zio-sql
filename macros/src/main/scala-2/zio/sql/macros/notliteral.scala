package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

sealed trait IsNotLiteral[F]

object IsNotLiteral {

  final case class FIsNotLiteral[F]() extends IsNotLiteral[F]

  implicit def materializeIsNotLiteral[F]: IsNotLiteral[F] =
    macro materializeIsNotLiteralImpl[F]

  def materializeIsNotLiteralImpl[F: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[IsNotLiteral[F]] = {
    import c.universe._
    val fType = weakTypeOf[F]

    def isLiteral(t: Type): Boolean =
      t.dealias match {
        case TypeRef(_, typeSymbol, _) if typeSymbol == symbolOf[zio.sql.Features.Literal] =>
          true
        case RefinedType(members, _)                                                       =>
          members.find(t => isLiteral(t)) match {
            case None    => false
            case Some(_) => true
          }
        case _                                                                             => false
      }

    if (isLiteral(fType)) {
      c.abort(c.enclosingPosition, "Use === instead of == when comparing two Exprs")
    } else {
      c.Expr[IsNotLiteral[F]](
        q"new zio.sql.macros.IsNotLiteral.FIsNotLiteral[${q"$fType"}]()"
      )
    }
  }
}
