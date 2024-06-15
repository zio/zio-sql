package zio.sql.mysql

import zio.Chunk
import zio.schema._
import zio.sql.{ SqlParameter, SqlRow, SqlStatement }
import zio.sql.driver.Renderer
import zio.sql.update._
import zio.sql.select._
import zio.sql.insert._
import zio.sql.delete._
import zio.sql.expr._
import zio.sql.table._
import zio.sql.typetag._

trait MysqlRenderModule extends MysqlSqlModule { self =>

  override def renderRead(read: Read[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderReadImpl(read)
    render.toString
  }

  override def renderInsert[A: Schema](insert: Insert[_, A]): SqlStatement = {
    implicit val render: Renderer = Renderer()
    val rows                      = MysqlRenderer.renderInsertImpl(insert)
    SqlStatement(render.toString, rows)
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderDeleteImpl(delete)
    render.toString
  }

  override def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderUpdateImpl(update)
    render.toString
  }

  object MysqlRenderer {
    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]): List[SqlRow] = {
      render("INSERT INTO ")
      renderTable(insert.table)

      render(" (")
      renderColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertPlaceholders(insert.values)

      buildInsertList(insert.values)
    }

    def renderDeleteImpl(delete: Delete[_])(implicit render: Renderer) = {
      render("DELETE FROM ")
      renderTable(delete.table)
      renderWhereExpr(delete.whereExpr)
    }

    def renderUpdateImpl(update: Update[_])(implicit render: Renderer) =
      update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ")
          renderTable(table)
          render(" SET ")
          renderSet(set)
          renderWhereExpr(whereExpr)
      }

    def renderReadImpl(read: Read[_])(implicit render: Renderer): Unit =
      read match {
        case Read.Mapped(read, _) => renderReadImpl(read)

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
          renderSelection(selection.value)
          table.foreach { t =>
            render(" FROM ")
            renderTable(t)
          }
          renderWhereExpr(whereExpr)
          groupByExprs match {
            case Read.ExprSet.ExprCons(_, _) =>
              render(" GROUP BY ")
              renderExprList(groupByExprs)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  render(" HAVING ")
                  renderExpr(havingExpr)
              }
            case Read.ExprSet.NoExpr         => ()
          }
          orderByExprs match {
            case _ :: _ =>
              render(" ORDER BY ")
              renderOrderingList(orderByExprs)
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
          render(" (", values.mkString(","), ") ") // todo fix needs escaping
      }

    private def buildInsertList[A](col: Seq[A])(implicit schema: Schema[A]): List[SqlRow] =
      col.foldLeft(List.empty[SqlRow]) { case (a, z) =>
        val row = schema.toDynamic(z) match {
          case DynamicValue.Record(_, listMap) =>
            val params = listMap.values.foldLeft(List.empty[SqlParameter]) { case (acc, el) =>
              acc ::: buildInsertRow(el)
            }
            SqlRow(params)
          case value                           =>
            SqlRow(buildInsertRow(value))
        }
        row :: a
      }

    private def buildInsertRow(dynValue: DynamicValue): List[SqlParameter] =
      dynValue match {
        case DynamicValue.Primitive(value, typeTag) =>
          // need to do this since StandardType is invariant in A
          StandardType.fromString(typeTag.tag) match {
            case Some(v) =>
              List(SqlParameter(v, value))
            case None    => Nil
          }
        case DynamicValue.Tuple(left, right)        =>
          buildInsertRow(left) ::: buildInsertRow(right)
        case DynamicValue.SomeValue(value)          => buildInsertRow(value)
        case DynamicValue.NoneValue                 =>
          List(SqlParameter(StandardType.UnitType, null))
        case DynamicValue.Sequence(chunk)           =>
          val bytes = chunk.map {
            case DynamicValue.Primitive(v, t) if t == StandardType.ByteType =>
              v.asInstanceOf[Byte]
            case _                                                          => throw new IllegalArgumentException("Only Byte sequences are supported.")
          }.toArray
          List(SqlParameter(StandardType.BinaryType, bytes))
        case _                                      => Nil
      }

    private def renderInsertPlaceholders[A](col: Seq[A])(implicit render: Renderer, schema: Schema[A]): Unit =
      col.toList match {
        case head :: _ =>
          render("(")
          renderInsertPlaceholder(head)
          render(");")
        case Nil       => ()
      }

    private def renderInsertPlaceholder[Z](z: Z)(implicit render: Renderer, schema: Schema[Z]): Unit =
      schema.toDynamic(z) match {
        case DynamicValue.Record(_, listMap) =>
          listMap.values.toList match {
            case head :: Nil  => renderPlaceholder(head)
            case head :: next =>
              renderPlaceholder(head)
              render(", ")
              renderPlaceholders(next)
            case Nil          => ()
          }
        case value                           => renderPlaceholder(value)
      }

    private def renderPlaceholders(dynValues: List[DynamicValue])(implicit render: Renderer): Unit =
      dynValues match {
        case head :: Nil  => renderPlaceholder(head)
        case head :: tail =>
          renderPlaceholder(head)
          render(", ")
          renderPlaceholders(tail)
        case Nil          => ()
      }

    def renderPlaceholder(dynValue: DynamicValue)(implicit render: Renderer): Unit =
      dynValue match {
        case DynamicValue.Primitive(_, _)    => render("?")
        case DynamicValue.Tuple(left, right) =>
          renderPlaceholder(left)
          render(", ")
          renderPlaceholder(right)
        case DynamicValue.SomeValue(value)   => renderPlaceholder(value)
        case DynamicValue.NoneValue          => render("?")
        case DynamicValue.Sequence(_)        => render("?")
        case _                               => ()
      }

    private def renderColumnNames(sources: SelectionSet[_])(implicit render: Renderer): Unit =
      sources match {
        case SelectionSet.Empty                       => ()
        case SelectionSet.Cons(columnSelection, tail) =>
          val _ = columnSelection.name.map { name =>
            render(name)
          }
          tail.asInstanceOf[SelectionSet[_]] match {
            case SelectionSet.Empty             => ()
            case next @ SelectionSet.Cons(_, _) =>
              render(", ")
              renderColumnNames(next.asInstanceOf[SelectionSet[_]])(render)
          }
      }

    private def renderSet(set: List[Set[_, _]])(implicit render: Renderer): Unit =
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
        case Nil          => // TODO restrict Update to not allow empty set
      }

    private def renderTable(table: Table)(implicit render: Renderer): Unit =
      table match {
        case Table.DialectSpecificTable(_)           => ???
        // The outer reference in this type test cannot be checked at run time?!
        case sourceTable: Table.Source               =>
          render(sourceTable.name)
        case Table.DerivedTable(read, name)          =>
          render(" ( ")
          renderRead(read.asInstanceOf[Read[_]])
          render(" ) ")
          render(name)
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

    private[zio] def quoted(name: String): String = "\"" + name + "\""

    private def renderExpr[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                    =>
        render(" (")
        renderRead(subselect)
        render(") ")
      case Expr.Source(table, column)                                                   =>
        (table, column.name) match {
          case (tableName: String, Some(columnName)) => render(tableName, ".", columnName)
          case _                                     => ()
        }
      case Expr.Unary(base, op)                                                         =>
        render(" ", op.symbol)
        renderExpr(base)
      case Expr.Property(base, op)                                                      =>
        renderExpr(base)
        render(" ", op.symbol)
      case Expr.Binary(left, right, op)                                                 =>
        renderExpr(left)
        render(" ", op.symbol, " ")
        renderExpr(right)
      case Expr.Relational(left, right, op)                                             =>
        renderExpr(left)
        render(" ", op.symbol, " ")
        renderExpr(right)
      case Expr.In(value, set)                                                          =>
        renderExpr(value)
        render(" IN ")
        if (set.isInstanceOf[Read.Subselect[_, _, _, _, _, _]]) {
          render("(")
          renderReadImpl(set)
          render(")")
        } else {
          renderReadImpl(set)
        }
      case lit: Expr.Literal[_]                                                         => renderLit(lit)
      case Expr.AggregationCall(p, aggregation)                                         =>
        render(aggregation.name.name, "(")
        renderExpr(p)
        render(")")
      case Expr.ParenlessFunctionCall0(fn)                                              =>
        val _ = render(fn.name)
      case Expr.FunctionCall0(fn)                                                       =>
        render(fn.name.name)
        render("(")
        val _ = render(")")
      case Expr.FunctionCall1(p, fn)                                                    =>
        render(fn.name.name, "(")
        renderExpr(p)
        render(")")
      case Expr.FunctionCall2(p1, p2, fn)                                               =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(")")
      case Expr.FunctionCall3(p1, p2, p3, fn)                                           =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(",")
        renderExpr(p3)
        render(")")
      case Expr.FunctionCall4(p1, p2, p3, p4, fn)                                       =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(",")
        renderExpr(p3)
        render(",")
        renderExpr(p4)
        render(")")
      case Expr.FunctionCall5(param1, param2, param3, param4, param5, function)         =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(")")
      case Expr.FunctionCall6(param1, param2, param3, param4, param5, param6, function) =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(",")
        renderExpr(param6)
        render(")")
      case Expr.FunctionCall7(
            param1,
            param2,
            param3,
            param4,
            param5,
            param6,
            param7,
            function
          ) =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(",")
        renderExpr(param6)
        render(",")
        renderExpr(param7)
        render(")")
    }

    private def renderLit[A, B](lit: Expr.Literal[_])(implicit render: Renderer): Unit = {
      import MysqlSpecific.MysqlTypeTag._
      import TypeTag._
      lit.typeTag match {
        case TDialectSpecific(tt) =>
          tt match {
            case tt @ TYear =>
              render(tt.cast(lit.value))
            case _          => ???
          }
        case TByteArray           =>
          render(
            lit.value.asInstanceOf[Chunk[Byte]].map("""\%02X""" format _).mkString("x'", "", "'")
          ) // todo fix `cast` infers correctly but map doesn't work for some reason
        case tt @ TChar =>
          render("'", tt.cast(lit.value), "'") // todo is this the same as a string? fix escaping
        case tt @ TInstant        =>
          render("TIMESTAMP '", tt.cast(lit.value), "'")
        case tt @ TLocalDate      =>
          render("DATE '", tt.cast(lit.value), "'")
        case tt @ TLocalDateTime  =>
          render("DATE '", tt.cast(lit.value), "'")
        case tt @ TLocalTime      =>
          render("TIME '", tt.cast(lit.value), "'")
        case tt @ TOffsetDateTime =>
          render("DATE '", tt.cast(lit.value), "'")
        case tt @ TOffsetTime     =>
          render("TIME '", tt.cast(lit.value), "'")
        case tt @ TUUID           =>
          render("'", tt.cast(lit.value), "'")
        case tt @ TZonedDateTime  =>
          render("DATE '", tt.cast(lit.value), "'")
        case TByte                =>
          render(lit.value)
        case TBigDecimal          =>
          render(lit.value)
        case TBoolean             =>
          render(lit.value)
        case TDouble              =>
          render(lit.value)
        case TFloat               =>
          render(lit.value)
        case TInt                 =>
          render(lit.value)
        case TLong                =>
          render(lit.value)
        case TShort               =>
          render(lit.value)
        case TString              =>
          render("'", lit.value, "'") // todo fix escaping
        case _ =>
          render(lit.value) // todo fix add TypeTag.Nullable[_] =>
      }
    }

    private def renderSelection[A](selectionSet: SelectionSet[A])(implicit render: Renderer): Unit =
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

    private def renderColumnSelection[A, B](columnSelection: ColumnSelection[A, B])(implicit render: Renderer): Unit =
      columnSelection match {
        case ColumnSelection.Constant(value, name) =>
          render(value) // todo fix escaping
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
            case _          => () // todo what do we do if we don't have a name?
          }
      }

    private def renderExprList(expr: Read.ExprSet[_])(implicit render: Renderer): Unit =
      expr match {
        case Read.ExprSet.ExprCons(head, tail) =>
          renderExpr(head)
          tail.asInstanceOf[Read.ExprSet[_]] match {
            case Read.ExprSet.ExprCons(_, _) =>
              render(", ")
              renderExprList(tail.asInstanceOf[Read.ExprSet[_]])
            case Read.ExprSet.NoExpr         => ()
          }
        case Read.ExprSet.NoExpr               => ()
      }

    private def renderOrderingList(expr: List[Ordering[Expr[_, _, _]]])(implicit render: Renderer): Unit =
      expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  =>
              renderExpr(value)
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

    /**
    * Drops the initial Litaral(true) present at the start of every WHERE expressions by default
    * and proceeds to the rest of Expr's.
    */
    private def renderWhereExpr[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Literal(true)   => ()
      case Expr.Binary(_, b, _) =>
        render(" WHERE ")
        renderExpr(b)
      case _                    =>
        render(" WHERE ")
        renderExpr(expr)
    }
  }

}
