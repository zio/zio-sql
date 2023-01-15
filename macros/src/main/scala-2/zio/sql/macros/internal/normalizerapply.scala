package zio.sql.macros.internal

import scala.reflect.macros.whitebox
import scala.collection.immutable

object NormalizerApply {
  //def applyNormalizerImpl[In: c.WeakTypeTag, Out: c.WeakTypeTag](
  def applyNormalizerImpl[In](
    c: whitebox.Context
  )(in: c.Expr[In]): c.Tree = {
    import c.universe._

    def unnest(trees: List[Tree]): List[Literal] = {
      trees match {
        case head :: Nil   => {
          head match {
            case Literal           => Nil
          // TODO uncomment
          //  case q"$expr[..$tpts]" => unnest(head.children.tail)
            case _                 => extractHead(head) 
          }
        }
                  
        case head :: tail  => extractHead(head) ++ unnest(tail)
        case immutable.Nil => c.abort(c.enclosingPosition, s"unexpected")
      }
    }

    def extractHead(head: Tree): List[Literal] = 
      head match {
        case l: Literal => List(l) 
        case q"()"      => Nil
        case s          => c.abort(c.enclosingPosition, s"unexpected ${s}")
      } 

    c.echo(c.enclosingPosition, s"ww ${in.tree}")

    val literals = unnest(in.tree.children.tail)

   // c.Expr[Out](
      q"..$literals"
    //)
  }
}
