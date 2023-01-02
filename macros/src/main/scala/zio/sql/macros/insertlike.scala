package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

sealed trait InsertLike[F, ColsRepr, AllColumnIdentities, Z]

/**
  * Macro that creates the instance of InsertLike sealed trait in case:
    1. We are inserting to all required (non optional) columns from table
    2. Types and order of inserting values match the columns
    3. Macro does not handle the case when we insert columns from different table. That is handled before we call macro by Insert.scala
  */  
object InsertLike {

  final case class CanBeInserted[F, ColsRepr, AllColumnIdentities, Z]()
      extends InsertLike[F, ColsRepr, AllColumnIdentities, Z]

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
          extractHead(t.args.headOption)
            .flatMap(h => extractHead(t.args.tail.headOption).map(t => (h.dealias -> t.dealias)))
        }
        case _ => Nil
      }

    def extractHead(headOption : Option[Type]): List[Type] =  headOption match {
        case Some(value) => List(value)
        case None        => Nil
    }

    def extractSingletons(f: Type): List[Type] =
      f.dealias match {
        case TypeRef(_, typeSymbol, args) 
            if typeSymbol == symbolOf[zio.sql.Features.Source[_, _]] => {
                List(args.head.dealias)
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
        case t @ TypeRef(pre, sym, args) if sym == symbolOf[Option[_]] => {
          // to avoid mismatch between Option]scala.predef.String] vs Option[java.lang.String]
          List(internal.typeRef(pre, sym, args.map(_.dealias)))
        }
        case t: TypeRef                                 => List(t.dealias)
        case _ => Nil
      }

    def extractValueTypes(toInsert: Type) =
      if (toInsert.typeSymbol.asClass.isCaseClass) {
        toInsert.dealias match {
          // for case classes
          case TypeRef(_, _, types) if types == Nil =>
            toInsert.decls.sorted.collect {
              case p: TermSymbol if p.isCaseAccessor && !p.isMethod =>
                p.typeSignature match {
                  case t @ TypeRef(pre, sym, args) if optionOrSome.contains(sym) =>            
                    // dealias inner type like Option]scala.predef.String] vs Option[java.lang.String]
                    internal.typeRef(pre, sym, args.map(_.dealias))
                  case x => x.dealias
                }
            }

          // for single options like Some("x")
          case TypeRef(pre, sym, args) if optionOrSome.contains(sym) => {
            // maps Some[_] to Option[_], dealias inner type
            List(internal.typeRef(pre, symbolOf[Option[_]], args.map(_.dealias)))
          }
          // for tuples
          case TypeRef(_, sym, types) => {
            types.map(t => t match {
              case t @ TypeRef(pre, sym, args) if optionOrSome.contains(sym) =>            
                // maps Some[_] to Option[_], dealias inner type
                internal.typeRef(pre, symbolOf[Option[_]], args.map(_.dealias))

              case x => x.dealias
            })
          }
          case _ => Nil
        }
      } else {
        toInsert match {
          // for inserting single primitve value
          case t: TypeRef => List(t)
          case _          => c.abort(c.enclosingPosition, s"Insert possible only with tuple, case class or a single value.")
        }
      }

    def optionOrSome = List(symbolOf[Some[_]], symbolOf[Option[_]])

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
          s"You need to insert to all non nullable columns. \n" +
            s"Null value in columns '${notInserted.keySet.mkString(" and ")}' violates not-null constraint"
        )
      }
    }

    if (values.size > colsRepr.size) {
        c.abort(c.enclosingPosition, s"INSERT has more expressions than target columns")
    }

    if (values.size < colsRepr.size) {
        c.abort(c.enclosingPosition, s"INSERT has more target columns than expressions")
    }
    
    val (vals, cols) = values.zip(colsRepr).flatMap {
      case (input, col) if (input == typeOf[None.type] && col.typeSymbol == symbolOf[Option[_]]) => {
        List((col, col))
      }
      case (input, col) =>  List((input, col))
    }.unzip


    if (vals != cols) {
      c.abort(c.enclosingPosition, s"Order of types of columns don't match the inserted values. \n" +
        s"Column types: ${cols.mkString(", ")}\n" +
        s"Inserted types: ${vals.mkString(", ")}")
    }

    c.Expr[InsertLike[F, ColsRepr, AllColumnIdentities, Z]](
      q"new zio.sql.macros.InsertLike.CanBeInserted[${q"$featuresType"}, ${q"$colsReprType"}, ${q"$allIdentitiesType"}, ${q"$valuesType"}]()"
    )

  }

}