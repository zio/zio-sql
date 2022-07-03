package zio.sql.mysql

import java.time._
import java.time.format.DateTimeFormatter

import zio.Chunk
import zio.schema._
import zio.schema.StandardType._
import zio.sql.driver.Renderer

trait MysqlRenderModule extends MysqlSqlModule { self =>

  override def renderRead(read: self.Read[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderReadImpl(read)
    render.toString
  }

  override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderInsertImpl(insert)
    render.toString
  }

  override def renderDelete(delete: self.Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderDeleteImpl(delete)
    render.toString
  }

  override def renderUpdate(update: self.Update[_]): String = {
    implicit val render: Renderer = Renderer()
    MysqlRenderer.renderUpdateImpl(update)
    render.toString
  }

  object MysqlRenderer {
    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]) = {
      render("INSERT INTO ")
      renderTable(insert.table)

      render(" (")
      renderColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertValues(insert.values)
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

    def renderReadImpl(read: self.Read[_])(implicit render: Renderer): Unit =
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

    private def renderInsertValues[A](col: Seq[A])(implicit render: Renderer, schema: Schema[A]): Unit =
      col.toList match {
        case head :: Nil  =>
          render("(")
          renderInsertValue(head)
          render(");")
        case head :: next =>
          render("(")
          renderInsertValue(head)(render, schema)
          render(" ),")
          renderInsertValues(next)
        case Nil          => ()
      }

    private def renderInsertValue[Z](z: Z)(implicit render: Renderer, schema: Schema[Z]): Unit =
      schema.toDynamic(z) match {
        case DynamicValue.Record(listMap) =>
          listMap.values.toList match {
            case head :: Nil  => renderDynamicValue(head)
            case head :: next =>
              renderDynamicValue(head)
              render(", ")
              renderDynamicValues(next)
            case Nil          => ()
          }
        case value                        => renderDynamicValue(value)
      }

    private def renderDynamicValues(dynValues: List[DynamicValue])(implicit render: Renderer): Unit =
      dynValues match {
        case head :: Nil  => renderDynamicValue(head)
        case head :: tail =>
          renderDynamicValue(head)
          render(", ")
          renderDynamicValues(tail)
        case Nil          => ()
      }

    def renderDynamicValue(dynValue: DynamicValue)(implicit render: Renderer): Unit =
      dynValue match {
        case DynamicValue.Primitive(value, typeTag) =>
          StandardType.fromString(typeTag.tag) match {
            case Some(v) =>
              v match {
                case BigDecimalType                             =>
                  render(value)
                case StandardType.InstantType(formatter)        =>
                  render(s"'${formatter.format(value.asInstanceOf[Instant])}'")
                case CharType                                   => render(s"'${value}'")
                case IntType                                    => render(value)
                case StandardType.MonthDayType                  => render(s"'${value}'")
                case BinaryType                                 => render(s"'${value}'")
                case StandardType.MonthType                     => render(s"'${value}'")
                case StandardType.LocalDateTimeType(formatter)  =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalDateTime])}'")
                case UnitType                                   => render("null") // None is encoded as Schema[Unit].transform(_ => None, _ => ())
                case StandardType.YearMonthType                 => render(s"'${value}'")
                case DoubleType                                 => render(value)
                case StandardType.YearType                      => render(s"'${value}'")
                case StandardType.OffsetDateTimeType(formatter) =>
                  render(s"'${formatter.format(value.asInstanceOf[OffsetDateTime])}'")
                case StandardType.ZonedDateTimeType(_)          =>
                  render(s"'${DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[ZonedDateTime])}'")
                case BigIntegerType                             => render(s"'${value}'")
                case UUIDType                                   => render(s"'${value}'")
                case StandardType.ZoneOffsetType                => render(s"'${value}'")
                case ShortType                                  => render(value)
                case StandardType.LocalTimeType(formatter)      =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalTime])}'")
                case StandardType.OffsetTimeType(formatter)     =>
                  render(s"'${formatter.format(value.asInstanceOf[OffsetTime])}'")
                case LongType                                   => render(value)
                case StringType                                 => render(s"'${value}'")
                case StandardType.PeriodType                    => render(s"'${value}'")
                case StandardType.ZoneIdType                    => render(s"'${value}'")
                case StandardType.LocalDateType(formatter)      =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalDate])}'")
                case BoolType                                   => render(value)
                case DayOfWeekType                              => render(s"'${value}'")
                case FloatType                                  => render(value)
                case StandardType.DurationType                  => render(s"'${value}'")
              }
            case None    => ()
          }
        case DynamicValue.Tuple(left, right)        =>
          renderDynamicValue(left)
          render(", ")
          renderDynamicValue(right)
        case DynamicValue.SomeValue(value)          => renderDynamicValue(value)
        case DynamicValue.NoneValue                 => render("null")
        case _                                      => ()
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
        case sourceTable: self.Table.Source          =>
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

    private def renderExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                    =>
        render(" (")
        renderRead(subselect)
        render(") ")
      case Expr.Source(table, column)                                                   =>
        (table, column.name) match {
          case (tableName: TableName, Some(columnName)) => render(tableName, ".", columnName)
          case _                                        => ()
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
        renderReadImpl(set)
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

    private def renderLit[A, B](lit: self.Expr.Literal[_])(implicit render: Renderer): Unit = {
      import MysqlSpecific.MysqlTypeTag._
      import TypeTag._
      lit.typeTag match {
        case TDialectSpecific(tt) =>
          tt match {
            case tt @ TYear                       =>
              render(tt.cast(lit.value))
            case _: MysqlSpecific.MysqlTypeTag[_] => ???
          }
        case TByteArray           =>
          render(
            lit.value.asInstanceOf[Chunk[Byte]].map("""\%02X""" format _).mkString("x'", "", "'")
          ) // todo fix `cast` infers correctly but map doesn't work for some reason
        case tt @ TChar           =>
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
        case _                    =>
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
    private def renderWhereExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
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
