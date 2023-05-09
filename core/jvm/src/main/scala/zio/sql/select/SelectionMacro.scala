package zio.sql.select

import scala.reflect.macros.whitebox
import zio.sql.expr.{ Expr => ZExpr }

private[select] class SelectionMacro(val c: whitebox.Context) {
  import c.universe._

  def selectApplyMacro[F, Source](
    exprs: c.Expr[ZExpr[F, Source, _]]*
  )(implicit i1: c.WeakTypeTag[F], i2: c.WeakTypeTag[Source]): c.Tree = {

    val selection = buildSelection[F, Source](exprs.toList)

    q"""zio.sql.select.SelectBuilder(
            ${selection.tree}
        )"""
  }

  def subselectApplyMacro[F, Source, ParentTable](
    exprs: c.Expr[ZExpr[F, Source, _]]*
  )(implicit i1: c.WeakTypeTag[F], i2: c.WeakTypeTag[Source], i3: c.WeakTypeTag[ParentTable]): c.Tree = {

    val fType       = weakTypeOf[F]
    val sourceType  = weakTypeOf[Source]
    val parentTable = weakTypeOf[ParentTable]

    val selection = buildSelection[F, Source](exprs.toList)

    val selectionSetType = buildSelectionSetType(exprs.toList.map(e => e.actualType), sourceType)

    q"""zio.sql.select.SubselectBuilder[${q"$fType"}, ${q"$sourceType"}, ${q"${selectionSetType}"} , ${q"$parentTable"}](
            ${selection.tree}
    )"""
  }

  private def buildSelection[F, Source](exprs: List[c.Expr[ZExpr[F, Source, _]]]) = 
    exprs
      .map(e =>
        reify {
          zio.sql.expr.Expr.expToSelection(e.splice)
        }
      )
      .reduce[c.Expr[zio.sql.select.Selection[F, Source, _ <: SelectionSet[Source]]]] { case (ex1, ex2) =>
        reify {
          ex1.splice ++ ex2.splice
        }
      }

  private def getTableAndType(f: Type): (Type, Type) =
    f.dealias match {
      case TypeRef(_, typeSymbol, args) if args.size == 3 => (args(1), args(2))
      case e                                              => c.abort(c.enclosingPosition, s"Error extracting table and expr type: ${e}")
    }

  // Table type in subselect is the intersection type of parent table and sub table (not only subtable taken from the expr)  
  private def buildSelectionSetType(types: List[Type], parentTableType: Type): Tree =
    types match {
      case Nil          =>
        tq"zio.sql.select.SelectionSet.Empty"
      case head :: tail =>
        val (_, a) = getTableAndType(head)
        tq"zio.sql.select.SelectionSet.Cons[${q"$parentTableType"}, ${q"$a"}, ${buildSelectionSetType(tail, parentTableType)}]"
    }

  /*
        TODO remove comments

        // reify {
        //   new zio.sql.select.SubselectBuilder(
        //     selection.splice
        //   )
        // }.tree

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
