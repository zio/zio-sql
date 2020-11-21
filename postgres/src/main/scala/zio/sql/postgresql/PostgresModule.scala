package zio.sql.postgresql

import zio.sql.Jdbc

/**
 */
trait PostgresModule extends Jdbc { self =>

  object PostgresFunctionDef {
    val Sind = FunctionDef[Double, Double](FunctionName("sind"))
  }

  private def buildExpr[A, B](builder: StringBuilder, expr: self.Expr[_, A, B]): Unit = expr match {
    case Expr.Source(tableName, column)                               =>
      val _ = builder.append(tableName).append(".").append(column.name)
    case Expr.Unary(base, op)                                         =>
      val _ = builder.append(" ").append(op.symbol)
      buildExpr(builder, base)
    case Expr.Property(base, op)                                      =>
      buildExpr(builder, base)
      val _ = builder.append(" ").append(op.symbol)
    case Expr.Binary(left, right, op)                                 =>
      buildExpr(builder, left)
      builder.append(" ").append(op.symbol).append(" ")
      buildExpr(builder, right)
    case Expr.Relational(left, right, op)                             =>
      buildExpr(builder, left)
      builder.append(" ").append(op.symbol).append(" ")
      buildExpr(builder, right)
    case Expr.In(value, set)                                          =>
      buildExpr(builder, value)
      buildRead(builder, set)
    case Expr.Literal(value)                                          =>
      val _ = builder.append(value.toString) //todo fix escaping
    case Expr.AggregationCall(param, aggregation)                     =>
      builder.append(aggregation.name.name)
      builder.append("(")
      buildExpr(builder, param)
      val _ = builder.append(")")
    case Expr.FunctionCall1(param, function)                          =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(builder, param)
      val _ = builder.append(")")
    case Expr.FunctionCall2(param1, param2, function)                 =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(builder, param1)
      builder.append(",")
      buildExpr(builder, param2)
      val _ = builder.append(")")
    case Expr.FunctionCall3(param1, param2, param3, function)         =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(builder, param1)
      builder.append(",")
      buildExpr(builder, param2)
      builder.append(",")
      buildExpr(builder, param3)
      val _ = builder.append(")")
    case Expr.FunctionCall4(param1, param2, param3, param4, function) =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(builder, param1)
      builder.append(",")
      buildExpr(builder, param2)
      builder.append(",")
      buildExpr(builder, param3)
      builder.append(",")
      buildExpr(builder, param4)
      val _ = builder.append(")")
  }

  override def renderUpdate(update: self.Update[_]): String = {
    val builder = new StringBuilder

    def buildUpdateString[A <: SelectionSet[_]](update: self.Update[_]): Unit =
      update match {
        case Update(table, set, whereExpr) =>
          builder.append("UPDATE ")
          buildTable(table)
          builder.append("SET ")
          buildSet(set)
          builder.append("WHERE ")
          buildExpr(builder, whereExpr)
      }

    def buildTable(table: Table): Unit =
      table match {
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: self.Table.Source =>
          val _ = builder.append(sourceTable.name)
        case Table.Joined(_, left, _, _)    =>
          buildTable(left) //TODO restrict Update to only allow sourceTable
      }

    def buildSet[A <: SelectionSet[_]](set: List[Set[_, A]]): Unit =
      set match {
        case head :: tail =>
          buildExpr(builder, head.lhs)
          builder.append(" = ")
          buildExpr(builder, head.rhs)
          tail.foreach { setEq =>
            builder.append(", ")
            buildExpr(builder, setEq.lhs)
            builder.append(" = ")
            buildExpr(builder, setEq.rhs)
          }
        case Nil          => //TODO restrict Update to not allow empty set
      }

    buildUpdateString(update)
    builder.toString()
  }

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder
    buildRead(builder, read)
    builder.toString()
  }

  private def buildRead(builder: StringBuilder, read: self.Read[_]): Unit = {

    def buildReadString[A <: SelectionSet[_]](read: self.Read[_]): Unit =
      read match {
        case read0 @ Read.Select(_, _, _, _, _, _, _, _) =>
          object Dummy {
            type F
            type A
            type B <: SelectionSet[A]
          }
          val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.A, Dummy.B]]
          import read._

          builder.append("SELECT ")
          buildSelection(selection.value)
          builder.append(" FROM ")
          buildTable(table)
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  =>
              builder.append(" WHERE ")
              buildExpr(builder, whereExpr)
          }
          groupBy match {
            case _ :: _ =>
              builder.append(" GROUP BY ")
              buildExprList(groupBy)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  builder.append(" HAVING ")
                  buildExpr(builder, havingExpr)
              }
            case Nil    => ()
          }
          orderBy match {
            case _ :: _ =>
              builder.append(" ORDER BY ")
              buildOrderingList(orderBy)
            case Nil    => ()
          }
          limit match {
            case Some(limit) =>
              builder.append(" LIMIT ").append(limit)
            case None        => ()
          }
          offset match {
            case Some(offset) =>
              val _ = builder.append(" OFFSET ").append(offset)
            case None         => ()
          }

        case Read.Union(left, right, distinct) =>
          buildReadString(left)
          builder.append(" UNION ")
          if (!distinct) builder.append("ALL ")
          buildReadString(right)

        case Read.Literal(values) =>
          val _ = builder.append(" (").append(values.mkString(",")).append(") ") //todo fix needs escaping
      }

    def buildExprList(expr: List[Expr[_, _, _]]): Unit               =
      expr match {
        case head :: tail =>
          buildExpr(builder, head)
          tail match {
            case _ :: _ =>
              builder.append(", ")
              buildExprList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }
    def buildOrderingList(expr: List[Ordering[Expr[_, _, _]]]): Unit =
      expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  => buildExpr(builder, value)
            case Ordering.Desc(value) =>
              buildExpr(builder, value)
              builder.append(" DESC")
          }
          tail match {
            case _ :: _ =>
              builder.append(", ")
              buildOrderingList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def buildSelection[A](selectionSet: SelectionSet[A]): Unit =
      selectionSet match {
        case cons0 @ SelectionSet.Cons(_, _) =>
          object Dummy {
            type Source
            type A
            type B <: SelectionSet[Source]
          }
          val cons = cons0.asInstanceOf[SelectionSet.Cons[Dummy.Source, Dummy.A, Dummy.B]]
          import cons._
          buildColumnSelection(head)
          if (tail != SelectionSet.Empty) {
            builder.append(", ")
            buildSelection(tail)
          }
        case SelectionSet.Empty              => ()
      }

    def buildColumnSelection[A, B](columnSelection: ColumnSelection[A, B]): Unit =
      columnSelection match {
        case ColumnSelection.Constant(value, name) =>
          builder.append(value.toString()) //todo fix escaping
          name match {
            case Some(name) =>
              val _ = builder.append(" AS ").append(name)
            case None       => ()
          }
        case ColumnSelection.Computed(expr, name)  =>
          buildExpr(builder, expr)
          name match {
            case Some(name) =>
              Expr.exprName(expr) match {
                case Some(sourceName) if name != sourceName =>
                  val _ = builder.append(" AS ").append(name)
                case _                                      => ()
              }
            case _          => () //todo what do we do if we don't have a name?
          }
      }
    def buildTable(table: Table): Unit                                           =
      table match {
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: self.Table.Source          =>
          val _ = builder.append(sourceTable.name)
        case Table.Joined(joinType, left, right, on) =>
          buildTable(left)
          builder.append(joinType match {
            case JoinType.Inner      => " INNER JOIN "
            case JoinType.LeftOuter  => " LEFT JOIN "
            case JoinType.RightOuter => " RIGHT JOIN "
            case JoinType.FullOuter  => " OUTER JOIN "
          })
          buildTable(right)
          builder.append(" ON ")
          buildExpr(builder, on)
          val _ = builder.append(" ")
      }
    buildReadString(read)
  }
}
