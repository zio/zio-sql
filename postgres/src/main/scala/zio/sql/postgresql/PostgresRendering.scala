package zio.sql.postgresql

import zio.sql.Rendering
import zio.sql.rendering.{ Builder, Rendering }

trait PostgresRendering[-A] extends Rendering[A] { self: PostgresModule =>
  //todo split out to separate module
  override type SqlRendering[-B] = PostgresRendering[B]

  def renderDeleteImpl(delete: Delete[_])(implicit render: Builder) = {
    render("DELETE FROM ")
    renderTable(delete.table)
    delete.whereExpr match {
      case Expr.Literal(true) => ()
      case _                  =>
        render(" WHERE ")
        renderExpr(delete.whereExpr)
    }
  }

  def renderUpdateImpl(update: Update[_])(implicit render: Builder) =
    update match {
      case Update(table, set, whereExpr) =>
        render("UPDATE ")
        renderTable(table)
        render(" SET ")
        renderSet(set)
        render(" WHERE ")
        renderExpr(whereExpr)
    }

  def renderSet[A <: SelectionSet[_]](set: List[Set[_, A]])(implicit render: Builder): Unit =
    set match {
      case head :: tail =>
        renderExpr(head.lhs)
        render(" = ")
        renderExpr(head.rhs)
        tail.foreach { setEq =>
          render(", ")
          renderExpr(setEq.lhs)
          render(" = ")
          renderExpr(setEq.rhs)
        }
      case Nil          => //TODO restrict Update to not allow empty set
    }

  private[zio] def renderLit[A, B](lit: self.Expr.Literal[_])(implicit render: Builder): Unit = {
    import TypeTag._
    lit.typeTag match {
      case tt @ TByteArray      => render(tt.cast(lit.value))                     // todo still broken
      //something like? render(tt.cast(lit.value).map("\\\\%03o" format _).mkString("E\'", "", "\'"))
      case tt @ TChar           =>
        render("'", tt.cast(lit.value), "'") //todo is this the same as a string? fix escaping
      case tt @ TInstant        => render("TIMESTAMP '", tt.cast(lit.value), "'") //todo test
      case tt @ TLocalDate      => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TLocalDateTime  => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TLocalTime      => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TOffsetDateTime => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TOffsetTime     => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TUUID           => render(tt.cast(lit.value))                     // todo still broken
      case tt @ TZonedDateTime  => render(tt.cast(lit.value))                     // todo still broken

      case TByte       => render(lit.value)           //default toString is probably ok
      case TBigDecimal => render(lit.value)           //default toString is probably ok
      case TBoolean    => render(lit.value)           //default toString is probably ok
      case TDouble     => render(lit.value)           //default toString is probably ok
      case TFloat      => render(lit.value)           //default toString is probably ok
      case TInt        => render(lit.value)           //default toString is probably ok
      case TLong       => render(lit.value)           //default toString is probably ok
      case TShort      => render(lit.value)           //default toString is probably ok
      case TString     => render("'", lit.value, "'") //todo fix escaping

      case _ => render(lit.value) //todo fix add TypeTag.Nullable[_] =>
    }
  }

  private[zio] def renderExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Builder): Unit = expr match {
    case Expr.Source(tableName, column)         => render(tableName, ".", column.name)
    case Expr.Unary(base, op)                   =>
      render(" ", op.symbol)
      renderExpr(base)
    case Expr.Property(base, op)                =>
      renderExpr(base)
      render(" ", op.symbol)
    case Expr.Binary(left, right, op)           =>
      renderExpr(left)
      render(" ", op.symbol, " ")
      renderExpr(right)
    case Expr.Relational(left, right, op)       =>
      renderExpr(left)
      render(" ", op.symbol, " ")
      renderExpr(right)
    case Expr.In(value, set)                    =>
      renderExpr(value)
      renderReadImpl(set)
    case lit: Expr.Literal[_]                   => renderLit(lit)
    case Expr.AggregationCall(p, aggregation)   =>
      render(aggregation.name.name, "(")
      renderExpr(p)
      render(")")
    case Expr.ParenlessFunctionCall0(fn)        =>
      val _ = render(fn.name)
    case Expr.FunctionCall0(fn)                 =>
      render(fn.name.name)
      render("(")
      val _ = render(")")
    case Expr.FunctionCall1(p, fn)              =>
      render(fn.name.name, "(")
      renderExpr(p)
      render(")")
    case Expr.FunctionCall2(p1, p2, fn)         =>
      render(fn.name.name, "(")
      renderExpr(p1)
      render(",")
      renderExpr(p2)
      render(")")
    case Expr.FunctionCall3(p1, p2, p3, fn)     =>
      render(fn.name.name, "(")
      renderExpr(p1)
      render(",")
      renderExpr(p2)
      render(",")
      renderExpr(p3)
      render(")")
    case Expr.FunctionCall4(p1, p2, p3, p4, fn) =>
      render(fn.name.name, "(")
      renderExpr(p1)
      render(",")
      renderExpr(p2)
      render(",")
      renderExpr(p3)
      render(",")
      renderExpr(p4)
      render(")")
  }

  private[zio] def renderReadImpl[A <: SelectionSet[_]](read: self.Read[_])(implicit render: Builder): Unit =
    read match {
      case read0 @ Read.Select(_, _, _, _, _, _, _, _) =>
        object Dummy { type F; type A; type B <: SelectionSet[A] }
        val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.A, Dummy.B]]
        import read._

        render("SELECT ")
        renderSelection(selection.value)
        render(" FROM ")
        renderTable(table)
        whereExpr match {
          case Expr.Literal(true) => ()
          case _                  =>
            render(" WHERE ")
            renderExpr(whereExpr)
        }
        groupBy match {
          case _ :: _ =>
            render(" GROUP BY ")
            renderExprList(groupBy)

            havingExpr match {
              case Expr.Literal(true) => ()
              case _                  =>
                render(" HAVING ")
                renderExpr(havingExpr)
            }
          case Nil    => ()
        }
        orderBy match {
          case _ :: _ =>
            render(" ORDER BY ")
            renderOrderingList(orderBy)
          case Nil    => ()
        }
        limit match {
          case Some(limit) => render(" LIMIT ", limit)
          case None        => ()
        }
        offset match {
          case Some(offset) => render(" OFFSET ", offset)
          case None         => ()
        }

      case Read.Union(left, right, distinct) =>
        renderReadImpl(left)
        render(" UNION ")
        if (!distinct) render("ALL ")
        renderReadImpl(right)

      case Read.Literal(values) =>
        render(" (", values.mkString(","), ") ") //todo fix needs escaping
    }

  def renderExprList(expr: List[Expr[_, _, _]])(implicit render: Builder): Unit =
    expr match {
      case head :: tail =>
        renderExpr(head)
        tail match {
          case _ :: _ =>
            render(", ")
            renderExprList(tail)
          case Nil    => ()
        }
      case Nil          => ()
    }

  def renderOrderingList(expr: List[Ordering[Expr[_, _, _]]])(implicit render: Builder): Unit =
    expr match {
      case head :: tail =>
        head match {
          case Ordering.Asc(value)  => renderExpr(value)
          case Ordering.Desc(value) =>
            renderExpr(value)
            render(" DESC")
        }
        tail match {
          case _ :: _ =>
            render(", ")
            renderOrderingList(tail)
          case Nil    => ()
        }
      case Nil          => ()
    }

  def renderSelection[A](selectionSet: SelectionSet[A])(implicit render: Builder): Unit =
    selectionSet match {
      case cons0 @ SelectionSet.Cons(_, _) =>
        object Dummy {
          type Source
          type A
          type B <: SelectionSet[Source]
        }
        val cons = cons0.asInstanceOf[SelectionSet.Cons[Dummy.Source, Dummy.A, Dummy.B]]
        import cons._
        renderColumnSelection(head)
        if (tail != SelectionSet.Empty) {
          render(", ")
          renderSelection(tail)
        }
      case SelectionSet.Empty              => ()
    }

  def renderColumnSelection[A, B](columnSelection: ColumnSelection[A, B])(implicit render: Builder): Unit =
    columnSelection match {
      case ColumnSelection.Constant(value, name) =>
        render(value) //todo fix escaping
        name match {
          case Some(name) => render(" AS ", name)
          case None       => ()
        }
      case ColumnSelection.Computed(expr, name)  =>
        renderExpr(expr)
        name match {
          case Some(name) =>
            Expr.exprName(expr) match {
              case Some(sourceName) if name != sourceName => render(" AS ", name)
              case _                                      => ()
            }
          case _          => () //todo what do we do if we don't have a name?
        }
    }

  def renderTable(table: Table)(implicit render: Builder): Unit =
    table match {
      //The outer reference in this type test cannot be checked at run time?!
      case sourceTable: self.Table.Source          => render(sourceTable.name)
      case Table.Joined(joinType, left, right, on) =>
        renderTable(left)
        render(joinType match {
          case JoinType.Inner      => " INNER JOIN "
          case JoinType.LeftOuter  => " LEFT JOIN "
          case JoinType.RightOuter => " RIGHT JOIN "
          case JoinType.FullOuter  => " OUTER JOIN "
        })
        renderTable(right)
        render(" ON ")
        renderExpr(on)
        render(" ")
    }
}
