package zio.sql.postgresql

import zio.sql.rendering.{ Builder, LowPriorityRendering, Rendering }

trait PostgresRenderModule { self: PostgresModule =>
  trait PostgresRendering[-A] extends Rendering[A]
  //todo split out to separate module
  object PostgresRendering    extends LowPriorityRendering {
    implicit case object ColumnSelectionRendering extends PostgresRendering[ColumnSelection[_, _]] {
      override def apply(columnSelection: ColumnSelection[_, _])(implicit builder: Builder): Unit =
        columnSelection match {
          case ColumnSelection.Constant(value, name) =>
            render(value) //todo fix escaping
            name match {
              case Some(name) => render(" AS ", name)
              case None       => ()
            }
          case ColumnSelection.Computed(expr, name)  =>
            render(expr)
            name match {
              case Some(name) =>
                Expr.exprName(expr) match {
                  case Some(sourceName) if name != sourceName => render(" AS ", name)
                  case _                                      => ()
                }
              case _          => () //todo what do we do if we don't have a name?
            }
        }
    }
    implicit case object DeleteRendering          extends PostgresRendering[Delete[_]]             {
      override def apply(delete: Delete[_])(implicit builder: Builder): Unit = {
        render("DELETE FROM ", delete.table)
        delete.whereExpr match {
          case Expr.Literal(true) => ()
          case _                  => render(" WHERE ", delete.whereExpr)
        }
      }
    }
    implicit case object ExprListRendering        extends PostgresRendering[List[Expr[_, _, _]]]   {
      override def apply(expr: List[Expr[_, _, _]])(implicit builder: Builder): Unit = expr match {
        case head :: tail =>
          render(head)
          tail match {
            case _ :: _ => render(", ", tail)
            case Nil    => ()
          }
        case Nil          => ()
      }
    }
    implicit case object ExprRendering            extends PostgresRendering[Expr[_, _, _]]         {
      override def apply(expr: Expr[_, _, _])(implicit builder: Builder): Unit = expr match {
        case Expr.Source(tableName, column)         => render(tableName.toString, ".", column.name)
        case Expr.Unary(base, op)                   => render(" ", op.symbol, base)
        case Expr.Property(base, op)                => render(base, " ", op.symbol)
        case Expr.Binary(left, right, op)           => render(left, " ", op.symbol, " ", right)
        case Expr.Relational(left, right, op)       => render(left, " ", op.symbol, " ", right)
        case Expr.In(value, set)                    => render(value, set)
        case lit: Expr.Literal[_]                   => render(lit)
        case Expr.AggregationCall(p, aggregation)   => render(aggregation.name.name, "(", p, ")")
        case Expr.ParenlessFunctionCall0(fn)        => render(fn.name)
        case Expr.FunctionCall0(fn)                 => render(fn.name.name, "()")
        case Expr.FunctionCall1(p, fn)              => render(fn.name.name, "(", p, ")")
        case Expr.FunctionCall2(p1, p2, fn)         => render(fn.name.name, "(", p1, ",", p2, ")")
        case Expr.FunctionCall3(p1, p2, p3, fn)     => render(fn.name.name, "(", p1, ",", p2, ",", p3, ")")
        case Expr.FunctionCall4(p1, p2, p3, p4, fn) => render(fn.name.name, "(", p1, ",", p2, ",", p3, ",", p4, ")")
      }
    }
    implicit case object LiteralRendering         extends PostgresRendering[Expr.Literal[_]]       {
      override def apply(lit: Expr.Literal[_])(implicit builder: Builder): Unit = {
        import TypeTag._
        lit.typeTag match {
          case tt @ TByteArray      => render(tt.cast(lit.value).toString())          // todo still broken
          //something like? render(tt.cast(lit.value).map("\\\\%03o" format _).mkString("E\'", "", "\'"))
          case tt @ TChar           =>
            render("'", tt.cast(lit.value).toString, "'") //todo is this the same as a string? fix escaping
          case tt @ TInstant        => render("TIMESTAMP '", tt.cast(lit.value), "'") //todo test
          case tt @ TLocalDate      => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TLocalDateTime  => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TLocalTime      => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TOffsetDateTime => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TOffsetTime     => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TUUID           => render(tt.cast(lit.value).toString)            // todo still broken
          case tt @ TZonedDateTime  => render(tt.cast(lit.value).toString)            // todo still broken

          case TByte       => render(lit.value.toString)  //default toString is probably ok
          case TBigDecimal => render(lit.value.toString)  //default toString is probably ok
          case TBoolean    => render(lit.value.toString)  //default toString is probably ok
          case TDouble     => render(lit.value.toString)  //default toString is probably ok
          case TFloat      => render(lit.value.toString)  //default toString is probably ok
          case TInt        => render(lit.value.toString)  //default toString is probably ok
          case TLong       => render(lit.value.toString)  //default toString is probably ok
          case TShort      => render(lit.value.toString)  //default toString is probably ok
          case TString     => render("'", lit.value, "'") //todo fix escaping

          case _ => render(lit.value.toString) //todo fix add TypeTag.Nullable[_] =>
        }
      }
    }
    implicit case object OrderingListRendering    extends PostgresRendering[List[Expr[_, _, _]]]   {
      override def apply(expr: List[Ordering[Expr[_, _, _]]])(implicit builder: Builder): Unit = expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  => render(value)
            case Ordering.Desc(value) => render(value, " DESC")
          }
          tail match {
            case _ :: _ =>
              render(", ", tail)
            case Nil    => ()
          }
        case Nil          => ()
      }
    }
    implicit case object SetListRendering         extends PostgresRendering[List[Set[_, _]]]       {
      override def apply(set: List[Set[_, _]])(implicit builder: Builder): Unit = set match {
        case head :: tail =>
          render(head.lhs, " = ", head.rhs)
          tail.foreach(setEq => render(", ", setEq.lhs, " = ", setEq.rhs))
        case Nil          => //TODO restrict Update to not allow empty set
      }
    }
    implicit case object ReadRendering            extends PostgresRendering[Read[_]]               {
      override def apply(read: Read[_])(implicit builder: Builder): Unit = read match {
        case read0 @ Read.Select(_, _, _, _, _, _, _, _) =>
          object Dummy { type F; type A; type B <: SelectionSet[A] }
          val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.A, Dummy.B]]
          import read._

          render("SELECT ", selection.value, " FROM ", table)
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  => render(" WHERE ", whereExpr)
          }
          groupBy match {
            case _ :: _ =>
              render(" GROUP BY ", groupBy)
              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  => render(" HAVING ", havingExpr)
              }
            case Nil    => ()
          }
          orderBy match {
            case _ :: _ => render(" ORDER BY ", orderBy)
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
          render(left, " UNION ")
          if (!distinct) render("ALL ")
          render(right)

        case Read.Literal(values) =>
          render(" (", values.mkString(","), ") ") //todo fix needs escaping
      }
    }
    implicit case object SelectionSetRendering    extends PostgresRendering[SelectionSet[_]]       {
      override def apply(selectionSet: SelectionSet[_])(implicit builder: Builder): Unit = selectionSet match {
        case cons: SelectionSet.Cons[source, a, b] =>
          render(cons.head)
          if (cons.tail != SelectionSet.Empty) render(", ", cons.tail)
        case SelectionSet.Empty                    => ()
      }
    }
    implicit case object TableRendering           extends PostgresRendering[Table]                 {
      override def apply(table: Table)(implicit builder: Builder): Unit = table match {
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: Table.Source               => render(sourceTable.name.toString)
        case Table.Joined(joinType, left, right, on) =>
          render(left)
          render(joinType match {
            case JoinType.Inner      => " INNER JOIN "
            case JoinType.LeftOuter  => " LEFT JOIN "
            case JoinType.RightOuter => " RIGHT JOIN "
            case JoinType.FullOuter  => " OUTER JOIN "
          })
          render(right, " ON ", on, " ")
      }
    }
    implicit case object UpdateRendering          extends PostgresRendering[Update[_]]             {
      override def apply(update: Update[_])(implicit builder: Builder): Unit = update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ", table, " SET ", set, " WHERE ", whereExpr)
      }
    }
  }
}
