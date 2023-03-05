package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

/**
  * select Count(id)
            from orders
            group by customer_id
    
    select customer_id
            from orders   
    
    select Count(id)
            from orders   

    select customer_id
            from orders
            group by customer_id

    select customer_id, Count(id)
            from orders
            group by customer_id
  */
sealed trait GroupByLike[All, Grouped]

object GroupByLike {

  final case class CanBeGrouped[All, Grouped]() extends GroupByLike[All, Grouped]

  implicit def createGroupByLike[All, Grouped]: GroupByLike[All, Grouped] = macro createInsertLikeImpl[All, Grouped]

  def createInsertLikeImpl[All: c.WeakTypeTag, Grouped: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[GroupByLike[All, Grouped]] = {
    import c.universe._

    val allType     = weakTypeOf[All]
    val groupedType = weakTypeOf[Grouped]

    def splitIntersection(t: Type): List[Type] =
      t.dealias match {
        case t: RefinedType                                              =>
          t.parents.flatMap(s => splitIntersection(s))
        case TypeRef(_, sym, _) if sym.info.isInstanceOf[RefinedTypeApi] =>
          splitIntersection(sym.info)
        case t: TypeRef                                                  =>
          t.args.headOption match {
            case Some(value) => List(value.dealias)
            case None        => Nil
          }
        case _                                                           => Nil
      }

    def isThereAggregation(t: Type): Boolean =
      t.dealias match {
        case RefinedType(members, _)                                                             =>
          members.find(t => isThereAggregation(t)) match {
            case None    => false
            case Some(_) => true
          }
        case TypeRef(_, typeSymbol, _) if typeSymbol == symbolOf[zio.sql.Features.Aggregated[_]] =>
          true
        case _                                                                                   => false
      }

    def extractFromFeatures(f: Type): List[Type] =
      f.dealias match {
        case TypeRef(_, typeSymbol, args) if typeSymbol == symbolOf[zio.sql.Features.Source[_, _]] =>
          List(args.head.dealias)
        case RefinedType(members, _)                                                               =>
          members.flatMap { f =>
            extractFromFeatures(f.dealias)
          }
        case _                                                                                     =>
          Nil
      }

    //  EXAMPLE
    //   select(name, Sum(price))
    //     .from(productTable)
    //     .groupBy(name, price)

    // name & price
    val groupedByF = splitIntersection(groupedType)

    // name
    val notAggregatedF = extractFromFeatures(allType)

    // true
    val aggregateFunctionExists = isThereAggregation(allType)

    val result = c.Expr[GroupByLike[All, Grouped]](
      q"new zio.sql.macros.GroupByLike.CanBeGrouped[${q"$allType"}, ${q"$groupedType"}]()"
    )

    val partialAggregation = aggregateFunctionExists && !notAggregatedF.isEmpty

    // price
    // val _ = groupedByF diff notAggregatedF

    // Nil
    val missing = notAggregatedF diff groupedByF

    // group by not called
    if (groupedByF.isEmpty) {
      if (partialAggregation) {
        c.abort(
          c.enclosingPosition,
          s"Column(s) ${missing.distinct.mkString(" and ")} must appear in the GROUP BY clause or be used in an aggregate function"
        )
      } else {
        result
      }
      // group by called
    } else {
      if (!missing.isEmpty) {
        c.abort(
          c.enclosingPosition,
          s"Column(s) ${missing.distinct.mkString(" and ")} must appear in the GROUP BY clause or be used in an aggregate function"
        )
      } else {
        result
      }
    }
  }
}
