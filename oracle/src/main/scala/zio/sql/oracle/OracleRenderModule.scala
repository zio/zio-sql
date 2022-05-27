package zio.sql.oracle

import zio.schema.Schema
import zio.sql.driver.Renderer
import zio.sql.driver.Renderer.Extensions

trait OracleRenderModule extends OracleSqlModule { self =>

  override def renderDelete(delete: self.Delete[_]): String = {
    val builder = new StringBuilder
    buildDeleteString(delete, builder)
    builder.toString
  }

  override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = ???

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder
    buildReadString(read, builder)
    builder.toString()
  }

  override def renderUpdate(update: self.Update[_]): String = {
    implicit val render: Renderer = Renderer()
    OracleRender.renderUpdateImpl(update)
    render.toString
  }

  // TODO: to consider the refactoring and using the implicit `Renderer`, see `renderExpr` in `PostgresRenderModule`
  private def buildExpr[A, B](expr: self.Expr[_, A, B], builder: StringBuilder): Unit = expr match {
    case Expr.Subselect(subselect)                                                            =>
      builder.append(" (")
      builder.append(renderRead(subselect))
      val _ = builder.append(") ")
    case Expr.Source(table, column)                                                           =>
      (table, column.name) match {
        case (tableName: TableName, Some(columnName)) =>
          val _ = builder.append(tableName).append(".").append(columnName)
        case _                                        => ()
      }
    case Expr.Unary(base, op)                                                                 =>
      val _ = builder.append(" ").append(op.symbol)
      buildExpr(base, builder)
    case Expr.Property(base, op)                                                              =>
      buildExpr(base, builder)
      val opString = op match {
        case PropertyOp.IsNull | PropertyOp.IsNotNull => op.symbol
        case PropertyOp.IsTrue                        => "= 1"
        case PropertyOp.IsNotTrue                     => "= 0"
      }
      val _        = builder.append(" ").append(opString)
    case Expr.Binary(left, right, op)                                                         =>
      buildExpr(left, builder)
      builder.append(" ").append(op.symbol).append(" ")
      buildExpr(right, builder)
    case Expr.Relational(left, right, op)                                                     =>
      buildExpr(left, builder)
      builder.append(" ").append(op.symbol).append(" ")
      buildExpr(right, builder)
    case Expr.In(value, set)                                                                  =>
      buildExpr(value, builder)
      buildReadString(set, builder)
    case Expr.Literal(true)                                                                   =>
      val _ = builder.append("1")
    case Expr.Literal(false)                                                                  =>
      val _ = builder.append("0")
    case Expr.Literal(value)                                                                  =>
      val _ = builder.append(value.toString) // todo fix escaping
    case Expr.AggregationCall(param, aggregation)                                             =>
      builder.append(aggregation.name.name)
      builder.append("(")
      buildExpr(param, builder)
      val _ = builder.append(")")
    case Expr.ParenlessFunctionCall0(functionName)                                            =>
      val _ = builder.append(functionName.name)
    case Expr.FunctionCall0(function)                                                         =>
      builder.append(function.name.name)
      builder.append("(")
      val _ = builder.append(")")
    case Expr.FunctionCall1(param, function)                                                  =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall2(param1, param2, function)                                         =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall3(param1, param2, param3, function)                                 =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      builder.append(",")
      buildExpr(param3, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall4(param1, param2, param3, param4, function)                         =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      builder.append(",")
      buildExpr(param3, builder)
      builder.append(",")
      buildExpr(param4, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall5(param1, param2, param3, param4, param5, function)                 =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      builder.append(",")
      buildExpr(param3, builder)
      builder.append(",")
      buildExpr(param4, builder)
      builder.append(",")
      buildExpr(param5, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall6(param1, param2, param3, param4, param5, param6, function)         =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      builder.append(",")
      buildExpr(param3, builder)
      builder.append(",")
      buildExpr(param4, builder)
      builder.append(",")
      buildExpr(param5, builder)
      builder.append(",")
      buildExpr(param6, builder)
      val _ = builder.append(")")
    case Expr.FunctionCall7(param1, param2, param3, param4, param5, param6, param7, function) =>
      builder.append(function.name.name)
      builder.append("(")
      buildExpr(param1, builder)
      builder.append(",")
      buildExpr(param2, builder)
      builder.append(",")
      buildExpr(param3, builder)
      builder.append(",")
      buildExpr(param4, builder)
      builder.append(",")
      buildExpr(param5, builder)
      builder.append(",")
      buildExpr(param6, builder)
      builder.append(",")
      buildExpr(param7, builder)
      val _ = builder.append(")")
  }

  private def buildReadString(read: self.Read[_], builder: StringBuilder): Unit =
    read match {
      case Read.Mapped(read, _) => buildReadString(read, builder)

      case read0 @ Read.Subselect(_, _, _, _, _, _, _, _) =>
        object Dummy {
          type F
          type Repr
          type Source
          type Head
          type Tail <: SelectionSet[Source]
        }
        val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.Repr, Dummy.Source, Dummy.Head, Dummy.Tail]]
        import read._

        builder.append("SELECT ")
        buildSelection(selection.value, builder)
        table.foreach { t =>
          builder.append(" FROM ")
          buildTable(t, builder)
        }
        whereExpr match {
          case Expr.Literal(true) => ()
          case _                  =>
            builder.append(" WHERE ")
            buildExpr(whereExpr, builder)
        }
        groupByExprs match {
          case Read.ExprSet.ExprCons(_, _) =>
            builder.append(" GROUP BY ")
            buildExprList(groupByExprs, builder)

            havingExpr match {
              case Expr.Literal(true) => ()
              case _                  =>
                builder.append(" HAVING ")
                buildExpr(havingExpr, builder)
            }
          case Read.ExprSet.NoExpr         => ()
        }
        orderByExprs match {
          case _ :: _ =>
            builder.append(" ORDER BY ")
            buildOrderingList(orderByExprs, builder)
          case Nil    => ()
        }
        // NOTE: Limit doesn't exist in oracle 11g (>=12), for now replacing it with rownum keyword of oracle
        // Ref: https://przemyslawkruglej.com/archive/2013/11/top-n-queries-the-new-row-limiting-clause-11g-12c/
        limit match {
          case Some(limit) =>
            val _ = builder.append(" WHERE rownum <= ").append(limit)
          case None        => ()
        }
      // NOTE: Offset doesn't exist in oracle 11g (>=12)
      // offset match {
      //   case Some(offset) =>
      //     val _ = builder.append(" OFFSET ").append(offset).append(" ROWS ")
      //   case None         => ()
      // }

      case Read.Union(left, right, distinct) =>
        buildReadString(left, builder)
        builder.append(" UNION ")
        if (!distinct) builder.append("ALL ")
        buildReadString(right, builder)

      case Read.Literal(values) =>
        val _ = builder.append(" (").append(values.mkString(",")).append(") ") // todo fix needs escaping
    }

  private def buildExprList(expr: Read.ExprSet[_], builder: StringBuilder): Unit                   =
    expr match {
      case Read.ExprSet.ExprCons(head, tail) =>
        buildExpr(head, builder)
        tail.asInstanceOf[Read.ExprSet[_]] match {
          case Read.ExprSet.ExprCons(_, _) =>
            builder.append(", ")
            buildExprList(tail.asInstanceOf[Read.ExprSet[_]], builder)
          case Read.ExprSet.NoExpr         => ()
        }
      case Read.ExprSet.NoExpr               => ()
    }
  private def buildOrderingList(expr: List[Ordering[Expr[_, _, _]]], builder: StringBuilder): Unit =
    expr match {
      case head :: tail =>
        head match {
          case Ordering.Asc(value)  => buildExpr(value, builder)
          case Ordering.Desc(value) =>
            buildExpr(value, builder)
            builder.append(" DESC")
        }
        tail match {
          case _ :: _ =>
            builder.append(", ")
            buildOrderingList(tail, builder)
          case Nil    => ()
        }
      case Nil          => ()
    }

  private def buildSelection[A](selectionSet: SelectionSet[A], builder: StringBuilder): Unit =
    selectionSet match {
      case cons0 @ SelectionSet.Cons(_, _) =>
        object Dummy {
          type Source
          type A
          type B <: SelectionSet[Source]
        }
        val cons = cons0.asInstanceOf[SelectionSet.Cons[Dummy.Source, Dummy.A, Dummy.B]]
        import cons._
        buildColumnSelection(head, builder)
        if (tail != SelectionSet.Empty) {
          builder.append(", ")
          buildSelection(tail, builder)
        }
      case SelectionSet.Empty              => ()
    }

  private def buildColumnSelection[A, B](columnSelection: ColumnSelection[A, B], builder: StringBuilder): Unit =
    columnSelection match {
      case ColumnSelection.Constant(value, name) =>
        builder.append(value.toString()) // todo fix escaping
        name match {
          case Some(name) =>
            val _ = builder.append(" AS ").append(name)
          case None       => ()
        }
      case ColumnSelection.Computed(expr, name)  =>
        buildExpr(expr, builder)
        name match {
          case Some(name) =>
            Expr.exprName(expr) match {
              case Some(sourceName) if name != sourceName =>
                val _ = builder.append(" AS ").append(name)
              case _                                      => ()
            }
          case _          => () // todo what do we do if we don't have a name?
        }
    }
  private def buildTable(table: Table, builder: StringBuilder): Unit                                           =
    table match {
      case Table.DialectSpecificTable(_)           => ???
      // The outer reference in this type test cannot be checked at run time?!
      case sourceTable: self.Table.Source          =>
        val _ = builder.append(sourceTable.name)
      case Table.DerivedTable(read, name)          =>
        builder.append(" ( ")
        builder.append(renderRead(read.asInstanceOf[Read[_]]))
        builder.append(" ) ")
        val _ = builder.append(name)
      case Table.Joined(joinType, left, right, on) =>
        buildTable(left, builder)
        builder.append(joinType match {
          case JoinType.Inner      => " INNER JOIN "
          case JoinType.LeftOuter  => " LEFT JOIN "
          case JoinType.RightOuter => " RIGHT JOIN "
          case JoinType.FullOuter  => " OUTER JOIN "
        })
        buildTable(right, builder)
        builder.append(" ON ")
        buildExpr(on, builder)
        val _ = builder.append(" ")
    }

  private def buildDeleteString(delete: Delete[_], builder: StringBuilder) = {
    builder.append("DELETE FROM ")
    buildTable(delete.table, builder)
    delete.whereExpr match {
      case Expr.Literal(true) => ()
      case _                  =>
        builder.append(" WHERE ")
        buildExpr(delete.whereExpr, builder)
    }
  }


  private[oracle] object OracleRender {

    def renderUpdateImpl(update: Update[_])(implicit render: Renderer): Unit =
      update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ")
          buildTable(table, render.builder)
          render(" SET ")
          renderSet(set)
          render(" WHERE ")
          buildExpr(whereExpr, render.builder)
      }

    def renderSet(set: List[Set[_, _]])(implicit render: Renderer): Unit =
      set match {
        case head :: tail =>
          renderSetLhs(head.lhs)
          render(" = ")
          buildExpr(head.rhs, render.builder)
          tail.foreach { setEq =>
            render(", ")
            renderSetLhs(setEq.lhs)
            render(" = ")
            buildExpr(setEq.rhs, render.builder)
          }
        case Nil          => // TODO restrict Update to not allow empty set
      }

    private[zio] def renderSetLhs[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit =
      expr match {
        // TODO: to check if Oracle allows for `tableName.columnName = value` format in update statement,
        //  or it requires `columnName = value` instead?
        case Expr.Source(_, column) =>
          column.name match {
            case Some(columnName) => render(columnName.quoted)
            case _                => ()
          }
        case _                      => ()
      }
  }
}
