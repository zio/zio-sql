package zio.sql.macros

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

sealed trait Normalizer[In] {
  type Out
}

object Normalizer {

  final case class Instance[In, Out2]() extends Normalizer[In] {
    override type Out = Out2
  }

  implicit def createNormalizer[In, Out]: Instance[In, Out] = macro createNormalizerImpl[In, Out]

  def createNormalizerImpl[In: c.WeakTypeTag, Out: c.WeakTypeTag](
    c: whitebox.Context
  ): c.Tree = {
    import c.universe._

    val inType = weakTypeOf[In]
    val _      = weakTypeOf[Out]

    def deconstructType(t: Type): List[Type] =
      t.dealias match {
        case TypeRef(_, y, types) if (types != Nil && (y == symbolOf[scala.Tuple2[_, _]])) =>
          types.head :: deconstructType(types.tail.head)
        case TypeRef(_, _, types) if (types == Nil)                                        =>
          Nil
        case s                                                                             =>
          c.abort(c.enclosingPosition, s"Error ${showRaw(s)}")
      }

    val values  = deconstructType(inType)
    val outType = tq"(..$values)"

    q"""zio.sql.macros.Normalizer.Instance[${q"$inType"}, $outType]()"""
  }

}
