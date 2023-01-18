package zio.sql.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

sealed trait WhereIsSound[WhereF, GroupByF]

/**
   // WHERE not allowed. Use `HAVING` instead.
   select(amount)
     .from(productTable)
     .groupBy(amount)
     .having(amount > 10)
     .where(amount > 10)

     GroupByF = Any with Source["amount"]
     WhereF = Source["amount"] with Literal] 

    // Aggregate functions are not allowed in WHERE
    select(amount)
      .from(productTable)
      .where(Sum(price) > 200)

    WhereF = Aggregate[Source["price"]] with Literal]
    GroupByF = Any

  */
object WhereIsSound {

  // WhereF
  final case class WhereCanBeCalled[WhereF, GroupByF]() extends WhereIsSound[WhereF, GroupByF]

  implicit def materializeWhereIsSound[WhereF, GroupByF]: WhereIsSound[WhereF, GroupByF] =
    macro materializeWhereIsSoundImpl[WhereF, GroupByF]

  def materializeWhereIsSoundImpl[WhereF: c.WeakTypeTag, GroupByF: c.WeakTypeTag](
    c: blackbox.Context
  ): c.Expr[WhereIsSound[WhereF, GroupByF]] = {
    import c.universe._

    val groupedType = weakTypeOf[GroupByF]
    val whereF      = weakTypeOf[WhereF]

    // Source["age"] with Source["amount"] : List(Source["age"], Source["amount"])
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

    // Aggregate[Source["price"]] with Literal] : true
    // Source["price"]  with Literal]           : false
    // we split the intersection and look for Aggregated in F
    def isThereAggregation(t: Type): Boolean =
      t.dealias match {
        case TypeRef(_, typeSymbol, _) if typeSymbol == symbolOf[zio.sql.Features.Aggregated[_]] =>
          true
        case RefinedType(members, _)                                                             =>
          members.find(t => isThereAggregation(t)) match {
            case None    => false
            case Some(_) => true
          }
        case _                                                                                   => false
      }

    if (!splitIntersection(groupedType).isEmpty) {
      c.abort(c.enclosingPosition, "WHERE not allowed. Use `HAVING` instead.")
    }

    if (isThereAggregation(whereF)) {
      c.abort(c.enclosingPosition, "Aggregate functions are not allowed in WHERE")
    }

    c.Expr[WhereIsSound[WhereF, GroupByF]](
      q"new zio.sql.macros.WhereIsSound.WhereCanBeCalled[${q"$whereF"}, ${q"$groupedType"}]()"
    )
  }
}
