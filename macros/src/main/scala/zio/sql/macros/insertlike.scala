package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

sealed trait InsertLike[F, ColsRepr, AllColumnIdentities, Z]

/**
  * Macro that creates the instance of InsertLike sealed trait in case:
    1. We are inserting to all required (non optional) columns from table
    2. Types and order of inserting values match the columns

  Macro does not handle the case when we insert additional column or columsn from different table.
  This will be handled by non macro code (Sql.scala SelectionUtils, Insert.scala)
  */
object InsertLike {

  final case class CanBeInserted[F, ColsRepr, AllColumnIdentities, Z]()
      extends InsertLike[F, ColsRepr, AllColumnIdentities, Z]

      //TODO think about better error messages

  implicit def createInsertLike[
      F,
      ColsRepr,
      AllColumnIdentities,
      Z
  ]: InsertLike[
    F,
    ColsRepr,
    AllColumnIdentities,
    Z,
  ] = macro createInsertLikeImpl[F, ColsRepr, AllColumnIdentities, Z]

  def createInsertLikeImpl[
      F: c.WeakTypeTag,
      ColsRepr: c.WeakTypeTag,
      AllColumnIdentities: c.WeakTypeTag,
      Z: c.WeakTypeTag
  ](
      c: blackbox.Context
  ): c.Expr[InsertLike[F, ColsRepr, AllColumnIdentities, Z]] = {
    import c.universe._

    val featuresType = weakTypeOf[F]
    val allIdentitiesType = weakTypeOf[AllColumnIdentities]

    val valuesType = weakTypeOf[Z]
    val colsReprType = weakTypeOf[ColsRepr]

    def splitIntersection(t: Type): List[(Type, Type)] =
      t.dealias match {
        case t: RefinedType =>
          t.parents.flatMap(s => splitIntersection(s))
        case TypeRef(_, sym, _) if sym.info.isInstanceOf[RefinedTypeApi] =>
          splitIntersection(sym.info)
        case t: TypeRef => {
          // TODO fails to compile for 2.12
          List(t.args.head.dealias -> t.args.tail.head.dealias)
        }
        case _ => Nil
      }

    def extractSingletons(f: Type): List[Type] =
      f.dealias match {
        case TypeRef(_, typeSymbol, args) 
            if typeSymbol == symbolOf[zio.sql.Features.Source[_, _]] => {
                List(args.head)
            }
        case TypeRef(_, typeSymbol, args) 
            if typeSymbol == symbolOf[zio.sql.Features.Union[_, _]] => {
                args.flatMap(f => extractSingletons(f))
            }
        case _ => 
           c.abort(
            c.enclosingPosition,
            s"You can insert only to source columns."
          )
      }

    def flatColsRepr(colsRepr: Type): List[Type] =
      colsRepr.dealias match {
        case TypeRef(_, sym, args) if sym == symbolOf[Tuple2[_, _]] =>
          args.flatMap(a => flatColsRepr(a))
        case TypeRef(_, sym, _) if sym == symbolOf[Unit] => Nil
        case t: TypeRef                                  => List(t)
        case _ => Nil
      }

    def extractValueTypes(toInsert: Type) =
      if (toInsert.typeSymbol.asClass.isCaseClass) {
        toInsert.dealias match {
          // for case classes
          case TypeRef(_, _, types) if types == Nil =>
            toInsert.decls.sorted.collect {
              case p: TermSymbol if p.isCaseAccessor && !p.isMethod =>
                p.typeSignature.dealias
            }
          // for tuples
          case TypeRef(_, _, types) => types.map(_.dealias)
          case _ => Nil
        }
      } else {
        c.abort(c.enclosingPosition, s"Insert with tuple or case class.")
      }

    // List(("id", java.util.UUID), ("name", String), ("age", Int))
    val identities = splitIntersection(
      allIdentitiesType
    ) // from table structure

    // List("id", "name", "age")
    val features = extractSingletons(featuresType) // from selection

    // List(java.util.UUID, String, Int)
    val colsRepr = flatColsRepr(colsReprType) // from selection

    // List(java.util.UUID, String, Int)
    val values = extractValueTypes(valuesType) // to insert

    // List(("id", java.util.UUID), ("name", String), ("age", Int))
    val selection = features.zip(colsRepr)

    if (selection != identities) {
      val selected = selection.toMap

      val remaining = identities.toMap.filterNot { case (singleton, _) =>
        selected.isDefinedAt(singleton)
      }

      val notInserted = remaining.filterNot { case (_, tpe) =>
        tpe.typeSymbol == symbolOf[Option[_]]
      }

      if (!notInserted.isEmpty) {
        c.abort(
          c.enclosingPosition,
          s"You need to insert to all non nullable columns."
        )
      }
    }

    if (colsRepr != values) {
      c.abort(c.enclosingPosition, s"Order of columns or types of columns don't match the inserted values.")
    }

    c.Expr[InsertLike[F, ColsRepr, AllColumnIdentities, Z]](
      q"new zio.sql.macros.InsertLike.CanBeInserted[${q"$featuresType"}, ${q"$colsReprType"}, ${q"$allIdentitiesType"}, ${q"$valuesType"}]()"
    )

  }

}