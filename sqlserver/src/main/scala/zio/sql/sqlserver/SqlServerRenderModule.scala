package zio.sql.sqlserver

import java.time._
import java.time.format.{ DateTimeFormatter, DateTimeFormatterBuilder }

import zio.schema._
import zio.sql.{ SqlParameter, SqlRow, SqlStatement }
import zio.sql.delete._
import zio.sql.driver.Renderer
import zio.sql.expr._
import zio.sql.insert._
import zio.sql.ops.Operator._
import zio.sql.select._
import zio.sql.table._
import zio.sql.typetag._
import zio.sql.update._

trait SqlServerRenderModule extends SqlServerSqlModule { self =>

  override def renderRead(read: Read[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderReadImpl(read)
    render.toString
  }

  override def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderUpdateImpl(update)
    render.toString
  }

  override def renderInsert[A: Schema](insert: Insert[_, A]): SqlStatement = {
    implicit val render: Renderer = Renderer()
    val rows                      = SqlServerRenderer.renderInsertImpl(insert)
    SqlStatement(render.toString, rows)
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    SqlServerRenderer.renderDeleteImpl(delete)
    render.toString
  }

  object SqlServerRenderer {

    private val fmtDateTimeOffset = new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      .appendOffset("+HH:MM", "Z")
      .toFormatter()

    private val fmtTimeOffset = new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(DateTimeFormatter.ISO_LOCAL_TIME)
      .appendOffset("+HH:MM", "Z")
      .toFormatter()

    private[zio] def renderReadImpl(read: Read[_])(implicit render: Renderer): Unit =
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
          buildWhereExpr(whereExpr)
          groupByExprs match {
            case Read.ExprSet.ExprCons(_, _) =>
              render(" GROUP BY ")
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

    private def renderLit(lit: Expr.Literal[_])(implicit render: Renderer): Unit = {
      import TypeTag._
      val value = lit.value
      lit.typeTag match {
        case TInstant        =>
          render(s"'${DateTimeFormatter.ISO_INSTANT.format(value.asInstanceOf[Instant])}'")
        case TLocalTime      =>
          render(s"'${DateTimeFormatter.ISO_LOCAL_TIME.format(value.asInstanceOf[LocalTime])}'")
        case TLocalDate      =>
          render(s"'${DateTimeFormatter.ISO_LOCAL_DATE.format(value.asInstanceOf[LocalDate])}'")
        case TLocalDateTime  =>
          render(s"'${DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(value.asInstanceOf[LocalDateTime])}'")
        case TZonedDateTime  =>
          render(s"'${fmtDateTimeOffset.format(value.asInstanceOf[ZonedDateTime])}'")
        case TOffsetTime     =>
          render(s"'${fmtTimeOffset.format(value.asInstanceOf[OffsetTime])}'")
        case TOffsetDateTime =>
          render(s"'${fmtDateTimeOffset.format(value.asInstanceOf[OffsetDateTime])}'")

        case TBoolean =>
          val b = value.asInstanceOf[Boolean]
          if (b) {
            render('1')
          } else {
            render('0')
          }
        case TUUID    => render(s"'$value'")

        case TBigDecimal => render(value)
        case TByte       => render(value)
        case TDouble     => render(value)
        case TFloat      => render(value)
        case TInt        => render(value)
        case TLong       => render(value)
        case TShort      => render(value)

        case TChar   => render(s"N'$value'")
        case TString => render(s"N'$value'")

        case _ => render(s"'$value'")
      }
    }

    private def buildExpr[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                            =>
        render(" (")
        render(renderRead(subselect))
        render(") ")
      case Expr.Source(table, column)                                                           =>
        (table, column.name) match {
          case (tableName: String, Some(columnName)) =>
            render(tableName, ".", columnName)
          case _                                     => ()
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
        right.asInstanceOf[Expr[_, A, B]] match {
          case Expr.Literal(true)  => val _ = render("1")
          case Expr.Literal(false) => val _ = render("0")
          case otherValue          => buildExpr(otherValue)
        }
      case Expr.In(value, set)                                                                  =>
        buildExpr(value)
        render(" IN ")
        if (set.isInstanceOf[Read.Subselect[_, _, _, _, _, _]]) {
          render("(")
          renderReadImpl(set)
          render(")")
        } else {
          renderReadImpl(set)
        }
      case literal: Expr.Literal[_]                                                             => renderLit(literal)
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

    private def buildExprList(expr: Read.ExprSet[_])(implicit render: Renderer): Unit =
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

    private def buildOrderingList(expr: List[Ordering[Expr[_, _, _]]])(implicit render: Renderer): Unit =
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

    private def buildSelection(selectionSet: SelectionSet[_])(implicit render: Renderer): Unit =
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

    private def buildColumnSelection[A, B](columnSelection: ColumnSelection[A, B])(implicit render: Renderer): Unit =
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

    private def buildTable(table: Table)(implicit render: Renderer): Unit =
      table match {

        case Table.DerivedTable(read, name) =>
          render(" ( ")
          render(renderRead(read.asInstanceOf[Read[_]]))
          render(" ) ")
          render(name)

        case sourceTable: Table.Source =>
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
            case _                                                                                    => ???
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

    /**
      * Drops the initial Litaral(true) present at the start of every WHERE expressions by default 
      * and proceeds to the rest of Expr's.
      */
    private def buildWhereExpr[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Literal(true)   => ()
      case Expr.Binary(_, b, _) =>
        render(" WHERE ")
        buildExpr(b)
      case _                    =>
        render(" WHERE ")
        buildExpr(expr)
    }

    private def buildColumnNames(sources: SelectionSet[_])(implicit render: Renderer): Unit =
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
              buildColumnNames(next.asInstanceOf[SelectionSet[_]])(render)
          }
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

    // TODO render each type according to their specifics & test it
    private def renderPlaceholder(dynValue: DynamicValue)(implicit render: Renderer): Unit =
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

    private def buildSet(set: List[Set[_, _]])(implicit render: Renderer): Unit =
      set match {
        case head :: tail =>
          buildExpr(head.lhs)
          render(" = ")
          buildExpr(head.rhs)
          tail.foreach { setEq =>
            render(", ")
            buildExpr(setEq.lhs)
            render(" = ")
            buildExpr(setEq.rhs)
          }
        case Nil          => // TODO restrict Update to not allow empty set
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

    def renderDeleteImpl(delete: Delete[_])(implicit render: Renderer) = {
      render("DELETE FROM ")
      buildTable(delete.table)
      buildWhereExpr(delete.whereExpr)
    }

    def renderUpdateImpl(update: Update[_])(implicit render: Renderer) =
      update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ")
          buildTable(table)
          render(" SET ")
          buildSet(set)
          buildWhereExpr(whereExpr)
      }

    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]): List[SqlRow] = {
      render("INSERT INTO ")
      buildTable(insert.table)

      render(" (")
      buildColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertPlaceholders(insert.values)

      buildInsertList(insert.values)
    }
  }
}
