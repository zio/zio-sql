package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

/**
      * `HAVING` can only be called:
      *
      *  1. If its called with an aggregated function returning boolean like `Having Count(id) > 5`, 
      *     while all the previously selected columns appeared in group by clause.
      *  2. If its called with a normal expression returning boolean like `having customer_id = '636ae137-5b1a-4c8c-b11f-c47c624d9cdc``
      *     and all the previously selected columns appeared in group by clause.
      * 
      *    select  order_date, Count(id)
                from orders
                group by order_date, customer_id
                having customer_id = '60b01fc9-c902-4468-8d49-3c0f989def37'
                
            select Count(id)
                from orders
                group by customer_id
                having Count(id) > 5
                
            select customer_id
                from orders
                group by customer_id
                having Count(id) > 5
                
            select Count(id)
                from orders
                having Count(id) > 5
      */
sealed trait HavingIsSound[AllF, GroupByF, HavingF]

object HavingIsSound {

  final case class HavingCanBeCalled[AllF, GroupByF, HavingF]() extends HavingIsSound[AllF, GroupByF, HavingF]

  implicit def materializeHavingIsSound[AllF, GroupByF, HavingF]: HavingIsSound[AllF, GroupByF, HavingF] =
    macro materializeHavingIsSoundImpl[AllF, GroupByF, HavingF]

  def materializeHavingIsSoundImpl[AllF: c.WeakTypeTag, GroupByF: c.WeakTypeTag, HavingF: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[HavingIsSound[AllF, GroupByF, HavingF]] = {
    import c.universe._

    val allType     = weakTypeOf[AllF]
    val groupedType = weakTypeOf[GroupByF]
    val havingType  = weakTypeOf[HavingF]

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

    def extractSourceColumns(f: Type): List[Type] =
      f.dealias match {
        case TypeRef(_, typeSymbol, args) if typeSymbol == symbolOf[zio.sql.Features.Source[_, _]] =>
          List(args.head.dealias)
        case RefinedType(members, _)                                              =>
          members.flatMap(f => extractSourceColumns(f))
        case _                                                                                     =>
          Nil
      }

    val groupedByF      = splitIntersection(groupedType)
    val nonAggSelection = extractSourceColumns(allType)

    val notCovered = nonAggSelection diff groupedByF

    val havingSources = extractSourceColumns(havingType)

    val missing = havingSources diff groupedByF

    if (!notCovered.isEmpty) {
      c.abort(
        c.enclosingPosition,
        s"Column(s) ${notCovered.distinct.mkString(" and ")} must appear in the GROUP BY clause or be used in an aggregate function"
      )
    } else {
      if ((havingSources.isEmpty) || missing.isEmpty) {
        c.Expr[HavingIsSound[AllF, GroupByF, HavingF]](
          q"new zio.sql.macros.HavingIsSound.HavingCanBeCalled[${q"$allType"}, ${q"$groupedType"}, ${havingType}]()"
        )
      } else {
        c.abort(
          c.enclosingPosition,
          s"Column(s) ${missing.distinct.mkString(" and ")} must appear in the GROUP BY clause or be used in an aggregate function"
        )
      }
    }
  }
}
