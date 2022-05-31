package zio.sql.sqlserver

import zio.schema.Schema
import zio.sql.driver.Renderer
import zio.sql.driver.Renderer.Extensions

trait SqlServerRenderModule extends SqlServerSqlModule { self =>

  override def renderRead(read: self.Read[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderReadImpl(read)
    render.toString
  }

  override def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderUpdateImpl(update)
    render.toString
  }

  override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderInsertImpl(insert)
    render.toString
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderDeleteImpl(delete)
    render.toString
  }

  object SqlServerRenderer {

    private[zio] def renderReadImpl(read: self.Read[_])(implicit render: Renderer): Unit =
      read match {
        // case Read.Mapped(read, _) => renderReadImpl(read.asInstanceOf[Read[Out]])
        case Read.Mapped(read, _) => renderReadImpl(read)

        // todo offset (needs orderBy, must use fetch _instead_ of top)
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

          render("SELECT ")
          limit match {
            case Some(limit) =>
              render("TOP ", limit, " ")
            case None        => ()
          }
          buildSelection(selection.value)
          table.foreach { t =>
            render(" FROM ")
            buildTable(t)
          }
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  =>
              render(" WHERE ")
              buildExpr(whereExpr)
          }
          groupByExprs match {
            case Read.ExprSet.ExprCons(_, _) =>
              render(" GROUP BT ")
              buildExprList(groupByExprs)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  render(" HAVING ")
                  buildExpr(havingExpr)
              }
            case Read.ExprSet.NoExpr         => ()
          }
          orderByExprs match {
            case _ :: _ =>
              render(" ORDER BY ")
              buildOrderingList(orderByExprs)
            case Nil    => ()
          }

        case Read.Union(left, right, distinct) =>
          renderReadImpl(left)
          render(" UNION ")
          if (!distinct) render("ALL ")
          renderReadImpl(right)

        case Read.Literal(values) =>
          render(" (", values.mkString(","), ") ") // todo fix needs escaping
      }

    def buildExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                            =>
        render(" (")
        render(renderRead(subselect))
        render(") ")
      case Expr.Source(table, column)                                                           =>
        (table, column.name) match {
          case (tableName: TableName, Some(columnName)) =>
            render(tableName, ".", columnName)
          case _                                        => ()
        }
      case Expr.Unary(base, op)                                                                 =>
        render(" ", op.symbol)
        buildExpr(base)
      case Expr.Property(base, op)                                                              =>
        buildExpr(base)
        val symbol = op match {
          case PropertyOp.IsNull    => "is null"
          case PropertyOp.IsNotNull => "is not null"
          case PropertyOp.IsTrue    => "= 1"
          case PropertyOp.IsNotTrue => "= 0"
        }
        render(" ", symbol)
      case Expr.Binary(left, right, op)                                                         =>
        buildExpr(left)
        render(" ", op.symbol, " ")
        buildExpr(right)
      case Expr.Relational(left, right, op)                                                     =>
        buildExpr(left)
        render(" ", op.symbol, " ")
        buildExpr(right)
      case Expr.In(value, set)                                                                  =>
        buildExpr(value)
        renderReadImpl(set)
      case literal @ Expr.Literal(value)                                                        =>
        val lit = literal.typeTag match {
          case TypeTag.TBoolean        =>
            // MSSQL server variant of true/false
            if (value.asInstanceOf[Boolean]) {
              "0 = 0"
            } else {
              "0 = 1"
            }
          case TypeTag.TLocalDateTime  =>
            val x = value
              .asInstanceOf[java.time.LocalDateTime]
              .format(java.time.format.DateTimeFormatter.ofPattern("YYYY-MM-dd HH:mm:ss"))
            s"'$x'"
          case TypeTag.TZonedDateTime  =>
            val x = value
              .asInstanceOf[java.time.ZonedDateTime]
              .format(java.time.format.DateTimeFormatter.ofPattern("YYYY-MM-dd HH:mm:ss"))
            s"'$x'"
          case TypeTag.TOffsetDateTime =>
            val x = value
              .asInstanceOf[java.time.OffsetDateTime]
              .format(java.time.format.DateTimeFormatter.ofPattern("YYYY-MM-dd HH:mm:ss"))
            s"'$x'"
          case _                       => value.toString.singleQuoted
        }
        render(lit)
      case Expr.AggregationCall(param, aggregation)                                             =>
        render(aggregation.name.name)
        render("(")
        buildExpr(param)
        render(")")
      case Expr.ParenlessFunctionCall0(function)                                                =>
        render(function.name)
      case Expr.FunctionCall0(function)                                                         =>
        render(function.name.name)
        render("(")
        render(")")
      case Expr.FunctionCall1(param, function)                                                  =>
        render(function.name.name)
        render("(")
        buildExpr(param)
        render(")")
      case Expr.FunctionCall2(param1, param2, function)                                         =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(")")
      case Expr.FunctionCall3(param1, param2, param3, function)                                 =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(",")
        buildExpr(param3)
        render(")")
      case Expr.FunctionCall4(param1, param2, param3, param4, function)                         =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(",")
        buildExpr(param3)
        render(",")
        buildExpr(param4)
        render(")")
      case Expr.FunctionCall5(param1, param2, param3, param4, param5, function)                 =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(",")
        buildExpr(param3)
        render(",")
        buildExpr(param4)
        render(",")
        buildExpr(param5)
        render(")")
      case Expr.FunctionCall6(param1, param2, param3, param4, param5, param6, function)         =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(",")
        buildExpr(param3)
        render(",")
        buildExpr(param4)
        render(",")
        buildExpr(param5)
        render(",")
        buildExpr(param6)
        render(")")
      case Expr.FunctionCall7(param1, param2, param3, param4, param5, param6, param7, function) =>
        render(function.name.name)
        render("(")
        buildExpr(param1)
        render(",")
        buildExpr(param2)
        render(",")
        buildExpr(param3)
        render(",")
        buildExpr(param4)
        render(",")
        buildExpr(param5)
        render(",")
        buildExpr(param6)
        render(",")
        buildExpr(param7)
        render(")")
    }

    def buildExprList(expr: Read.ExprSet[_])(implicit render: Renderer): Unit                   =
      expr match {
        case Read.ExprSet.ExprCons(head, tail) =>
          buildExpr(head)
          tail.asInstanceOf[Read.ExprSet[_]] match {
            case Read.ExprSet.ExprCons(_, _) =>
              render(", ")
              buildExprList(tail.asInstanceOf[Read.ExprSet[_]])
            case Read.ExprSet.NoExpr         => ()
          }
        case Read.ExprSet.NoExpr               => ()
      }
    def buildOrderingList(expr: List[Ordering[Expr[_, _, _]]])(implicit render: Renderer): Unit =
      expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  => buildExpr(value)
            case Ordering.Desc(value) =>
              buildExpr(value)
              render(" desc")
          }
          tail match {
            case _ :: _ =>
              render(", ")
              buildOrderingList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def buildSelection(selectionSet: SelectionSet[_])(implicit render: Renderer): Unit =
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
            render(", ")
            buildSelection(tail)
          }
        case SelectionSet.Empty              => ()
      }

    def buildColumnSelection[A, B](columnSelection: ColumnSelection[A, B])(implicit render: Renderer): Unit =
      columnSelection match {
        case ColumnSelection.Constant(value, name) =>
          render(value.toString()) // todo fix escaping
          name match {
            case Some(name) =>
              render(" as ", name)
            case None       => ()
          }
        case ColumnSelection.Computed(expr, name)  =>
          buildExpr(expr)
          name match {
            case Some(name) =>
              Expr.exprName(expr) match {
                case Some(sourceName) if name != sourceName =>
                  render(" as ", name)
                case _                                      => ()
              }
            case _          => () // todo what do we do if we don't have a name?
          }
      }

    def buildTable(table: Table)(implicit render: Renderer): Unit =
      table match {

        case Table.DerivedTable(read, name) =>
          render(" ( ")
          render(renderRead(read.asInstanceOf[Read[_]]))
          render(" ) ")
          render(name)

        case sourceTable: self.Table.Source =>
          render(sourceTable.name)

        case Table.DialectSpecificTable(tableExtension) =>
          tableExtension match {
            case SqlServerSpecific.SqlServerTable.CrossOuterApplyTable(crossType, left, derivedTable) =>
              buildTable(left)

              crossType match {
                case SqlServerSpecific.SqlServerTable.CrossType.CrossApply => render(" cross apply ")
                case SqlServerSpecific.SqlServerTable.CrossType.OuterApply => render(" outer apply ")
              }

              val _ = buildTable(derivedTable)
          }

        case Table.Joined(joinType, left, right, on) =>
          buildTable(left)
          render(joinType match {
            case JoinType.Inner      => " inner join "
            case JoinType.LeftOuter  => " left join "
            case JoinType.RightOuter => " right join "
            case JoinType.FullOuter  => " outer join "
          })
          buildTable(right)
          render(" on ")
          buildExpr(on)
          render(" ")
      }

    def renderDeleteImpl(delete: Delete[_])(implicit render: Renderer) = {
      render("DELETE FROM ")
      buildTable(delete.table)
      delete.whereExpr match {
        case Expr.Literal(true) => ()
        case _                  =>
          render(" WHERE ")
          buildExpr(delete.whereExpr)
      }
    }

    // TODO https://github.com/zio/zio-sql/issues/160
    def renderUpdateImpl(update: Update[_])(implicit render: Renderer) =
      ???

    // TODO https://github.com/zio/zio-sql/issues/160
    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]) =
      ???
  }
}
