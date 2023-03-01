package zio.sql.postgresql

import java.time._
import java.time.format.DateTimeFormatter

import zio.Chunk
import zio.schema._
import zio.schema.StandardType._
import zio.sql.driver.Renderer
import java.time.format.DateTimeFormatter._
import zio.sql.update._
import zio.sql.select._
import zio.sql.insert._
import zio.sql.delete._
import zio.sql.expr._
import zio.sql.table._
import zio.sql.typetag._

trait PostgresRenderModule extends PostgresSqlModule { self =>

  override def renderRead(read: Read[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderer.renderReadImpl(read)
    render.toString
  }

  override def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderer.renderUpdateImpl(update)
    render.toString
  }

  override def renderInsert[A: Schema](insert: Insert[_, A]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderer.renderInsertImpl(insert)
    render.toString
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderer.renderDeleteImpl(delete)
    render.toString
  }

  object PostgresRenderer {

    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]) = {
      render("INSERT INTO ")
      renderTable(insert.table)

      render(" (")
      renderColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertValues(insert.values)
    }

    private def renderInsertValues[A](col: Seq[A])(implicit render: Renderer, schema: Schema[A]): Unit =
      // TODO any performance penalty because of toList ?
      col.toList match {
        case head :: Nil  =>
          render("(")
          renderInserValue(head)
          render(");")
        case head :: next =>
          render("(")
          renderInserValue(head)(render, schema)
          render(" ),")
          renderInsertValues(next)
        case Nil          => ()
      }

    private def renderInserValue[Z](z: Z)(implicit render: Renderer, schema: Schema[Z]): Unit =
      schema.toDynamic(z) match {
        case DynamicValue.Record(_, listMap) =>
          listMap.values.toList match {
            case head :: Nil  => renderDynamicValue(head)
            case head :: next =>
              renderDynamicValue(head)
              render(", ")
              renderDynamicValues(next)
            case Nil          => ()
          }
        case value                           => renderDynamicValue(value)
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

    // TODO render each type according to their specifics & test it
    private def renderDynamicValue(dynValue: DynamicValue)(implicit render: Renderer): Unit =
      dynValue match {
        case DynamicValue.Primitive(value, typeTag) =>
          // need to do this since StandardType is invariant in A
          StandardType.fromString(typeTag.tag) match {
            case Some(v) =>
              v match {
                case BigDecimalType                  =>
                  render(value)
                case StandardType.InstantType        =>
                  render(s"'${ISO_INSTANT.format(value.asInstanceOf[Instant])}'")
                case ByteType                        => render(s"'${value}'")
                case CharType                        => render(s"'${value}'")
                case IntType                         => render(value)
                case StandardType.MonthDayType       => render(s"'${value}'")
                case BinaryType                      => render(s"'${value}'")
                case StandardType.MonthType          => render(s"'${value}'")
                case StandardType.LocalDateTimeType  =>
                  render(s"'${ISO_LOCAL_DATE_TIME.format(value.asInstanceOf[LocalDateTime])}'")
                case UnitType                        => render("null") // None is encoded as Schema[Unit].transform(_ => None, _ => ())
                case StandardType.YearMonthType      => render(s"'${value}'")
                case DoubleType                      => render(value)
                case StandardType.YearType           => render(s"'${value}'")
                case StandardType.OffsetDateTimeType =>
                  render(s"'${ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[OffsetDateTime])}'")
                case StandardType.ZonedDateTimeType  =>
                  render(s"'${DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[ZonedDateTime])}'")
                case BigIntegerType                  => render(s"'${value}'")
                case UUIDType                        => render(s"'${value}'")
                case StandardType.ZoneOffsetType     => render(s"'${value}'")
                case ShortType                       => render(value)
                case StandardType.LocalTimeType      =>
                  render(s"'${DateTimeFormatter.ISO_LOCAL_TIME.format(value.asInstanceOf[LocalTime])}'")
                case StandardType.OffsetTimeType     =>
                  render(s"'${DateTimeFormatter.ISO_OFFSET_TIME.format(value.asInstanceOf[OffsetTime])}'")
                case LongType                        => render(value)
                case StringType                      => render(s"'${value}'")
                case StandardType.PeriodType         => render(s"'${value}'")
                case StandardType.ZoneIdType         => render(s"'${value}'")
                case StandardType.LocalDateType      =>
                  render(s"'${ISO_DATE.format(value.asInstanceOf[LocalDate])}'")
                case BoolType                        => render(value)
                case DayOfWeekType                   => render(s"'${value}'")
                case FloatType                       => render(value)
                case StandardType.DurationType       => render(s"'${value}'")
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
        case SelectionSet.Empty                       => () // table is a collection of at least ONE column
        case SelectionSet.Cons(columnSelection, tail) =>
          val _ = columnSelection.name.map { name =>
            render(quoted(name))
          }
          tail.asInstanceOf[SelectionSet[_]] match {
            case SelectionSet.Empty             => ()
            case next @ SelectionSet.Cons(_, _) =>
              render(", ")
              renderColumnNames(next.asInstanceOf[SelectionSet[_]])(render)
          }
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

    private def renderSet(set: List[Set[_, _]])(implicit render: Renderer): Unit =
      set match {
        case head :: tail =>
          renderSetLhs(head.lhs)
          render(" = ")
          renderExpr(head.rhs)
          tail.foreach { setEq =>
            render(", ")
            renderSetLhs(setEq.lhs)
            render(" = ")
            renderExpr(setEq.rhs)
          }
        case Nil          => // TODO restrict Update to not allow empty set
      }

    private[zio] def renderLit[A, B](lit: Expr.Literal[_])(implicit render: Renderer): Unit =
      renderLit(lit.typeTag, lit.value)

    private[zio] def renderLit(typeTag: TypeTag[_], value: Any)(implicit render: Renderer): Unit = {
      import TypeTag._
      import PostgresSpecific.PostgresTypeTag._
      typeTag match {
        case TShort                             => render(value)
        case TFloat                             => render(value)
        case TBoolean                           => render(value)
        case tt @ TOffsetDateTime               => render("DATE '", tt.cast(value), "'")
        case TDouble                            => render(value)
        case tt @ TChar                         => render("'", tt.cast(value), "'")
        case tt @ TLocalTime                    => render(tt.cast(value)) // todo still broken
        case TBigDecimal                        => render(value)
        case TByte                              => render(value)
        case tt @ TZonedDateTime                => render("DATE '", tt.cast(value), "'")
        case TString                            => render("'", value, "'")
        case TDialectSpecific(typeTagExtension) =>
          typeTagExtension match {
            case tt @ TInterval   => render(tt.cast(value))
            case tt @ TTimestampz => render(tt.cast(value))
            case _                => ???
          }
        case TByteArray                         =>
          render(
            value.asInstanceOf[Chunk[Byte]].map("""\%03o""".format(_)).mkString("E\'", "", "\'")
          )
        case tt @ TUUID                         => render("'", tt.cast(value), "'")
        case tt @ TLocalDate                    => render("DATE '", tt.cast(value), "'")
        case tt @ TOffsetTime                   => render(tt.cast(value)) // todo still broken
        case tt @ TInstant                      => render("TIMESTAMP '", tt.cast(value), "'")
        case TLong                              => render(value)
        case tt @ TLocalDateTime                => render("DATE '", tt.cast(value), "'")
        case TInt                               => render(value)
        case n: Nullable[_]                     =>
          renderLit(n.typeTag, value.asInstanceOf[Option[_]].get)
        case TNone                              =>
          render("null")
      }
    }

    /*
     * PostgreSQL doesn't allow for `tableName.columnName = value` format in update statement,
     * instead requires `columnName = value`.
     */
    private[zio] def renderSetLhs[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit =
      expr match {
        case Expr.Source(_, column) =>
          column.name match {
            case Some(columnName) => render(quoted(columnName))
            case _                => ()
          }
        case _                      => ()
      }

    private[zio] def quoted(name: String): String = "\"" + name + "\""

    private[zio] def renderExpr[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                    =>
        render(" (")
        render(renderRead(subselect))
        render(") ")
      case e @ Expr.Source(table, column)                                               =>
        (table, column.name) match {
          case (tableName: String, Some(columnName)) =>
            render(quoted(tableName), ".", quoted(columnName))
          case _                                     => ()
        }
        e.typeTag match {
          case TypeTag.TBigDecimal => render("::numeric")
          case _                   => ()
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

    private[zio] def renderReadImpl(read: Read[_])(implicit render: Renderer): Unit =
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
          val read =
            read0.asInstanceOf[Read.Subselect[Dummy.F, Dummy.Repr, Dummy.Source, Dummy.Source, Dummy.Head, Dummy.Tail]]
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

    private def renderTable(table: Table)(implicit render: Renderer): Unit =
      table match {
        case Table.DialectSpecificTable(tableExtension) =>
          tableExtension match {
            case PostgresSpecific.PostgresSpecificTable.LateraLTable(left, derivedTable) =>
              renderTable(left)

              render(" ,lateral ")

              renderTable(derivedTable)
            case _                                                                       => ???
          }
        // The outer reference in this type test cannot be checked at run time?!
        case sourceTable: Table.Source                  => render(quoted(sourceTable.name))
        case Table.DerivedTable(read, name)             =>
          render(" ( ")
          render(renderRead(read.asInstanceOf[Read[_]]))
          render(" ) ")
          render(name)
        case Table.Joined(joinType, left, right, on)    =>
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
