package zio.sql.select

import scala.reflect.macros.whitebox
import zio.sql.expr.{Expr => ZExpr}

private[select] class SelectionMacro(val c: whitebox.Context) {
  import c.universe._

    def selectApplyMacro[F, Source](
        exprs: c.Expr[ZExpr[F, Source, _]]*
      )(implicit i1: c.WeakTypeTag[F],
                 i2: c.WeakTypeTag[Source]): c.Tree = {

        val selection = exprs
            .toList
            .map(e => reify {
                zio.sql.expr.Expr.expToSelection(e.splice)
            })
            .reduce[c.Expr[zio.sql.select.Selection[F, Source, _ <: SelectionSet[Source]]]] {
                case (ex1, ex2) => {
                    reify {
                        ex1.splice ++ ex2.splice
                    }
                }
            }

         q"""zio.sql.select.SelectBuilder(
            ${selection.tree}
        )"""
    }



      /*
        ALT

        val fType = weakTypeOf[F]
        val sourceType = weakTypeOf[Source]

        c.abort(
          c.enclosingPosition,
          s"""${fType.dealias}"""
        )

        def extractExprType(f: Type): Type =
            f.dealias match {
                case TypeRef(_, typeSymbol, args) if args.size == 3 => args(2)
                case e => c.abort(c.enclosingPosition, s"Error extracting expr type: ${e}")
            }

        reify {
            new zio.sql.select.SelectBuilder(
                selection.splice
            )
        }.tree


        val _ = tq"zio.sql.select.SelectBuilder[${q"$fType"}, ${q"$sourceType"},  zio.sql.select.SelectionSet.Cons[${q"$sourceType"},java.util.UUID, zio.sql.select.SelectionSet.Cons[${q"$sourceType"}, Int,zio.sql.select.SelectionSet.Empty]]]"
        
        q"""zio.sql.select.SelectBuilder(
            ${selection.tree}
        )"""

        val _ = q"""new zio.sql.select.SelectBuilder(
          ${q"???.asInstanceOf[zio.sql.select.Selection[${q"$fType"}, ${q"$sourceType"}, zio.sql.select.SelectionSet.Cons[${q"$sourceType"},java.util.UUID, zio.sql.select.SelectionSet.Cons[${q"$sourceType"}, Int,zio.sql.select.SelectionSet.Empty]]]]"}
        )"""

        q"""new zio.sql.select.SelectBuilder(
          ${q"${selection.tree}"}
        )"""
      
      */
}
