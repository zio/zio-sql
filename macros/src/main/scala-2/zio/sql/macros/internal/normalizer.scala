package zio.sql.macros.internal

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

sealed trait Normalizer[In] {
  type Out

  //def apply(in: In): Out = macro NormalizerApply.applyNormalizerImpl[In]
}

object Normalizer {

  final case class Instance[In, Out2]() extends Normalizer[In] {
    override type Out = Out2
  }

  implicit def createNormalizer[In]: Normalizer[In] = macro createNormalizerImpl[In]

  def createNormalizerImpl[In: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[Normalizer[In]] = {
    import c.universe._

    val inType = weakTypeOf[In]

    def deconstructType(t: Type): List[Type] =
      t.dealias match {
        case TypeRef(_, _, types) if (types != Nil && types.tail.head == typeOf[Unit])     =>
          List(types.head)
        case TypeRef(_, y, types) if (types != Nil && (y == symbolOf[scala.Tuple2[_, _]])) =>
          types.head :: deconstructType(types.tail.head)
        // case TypeRef(_, y, _) if (y == symbolOf[Unit])     =>
        //   Nil
        case s                                                                             => 
          c.abort(c.enclosingPosition, s"Something went wrong ${showRaw(s)}")
      }

    val values  = deconstructType(inType)
    val outType = tq"(..$values)"

    c.Expr[Normalizer[In]](
      q"""zio.sql.macros.internal.Normalizer.Instance[${q"$inType"}, $outType]()"""
    )
  }

}
