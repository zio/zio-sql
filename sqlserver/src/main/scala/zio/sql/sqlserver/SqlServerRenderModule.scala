package zio.sql.sqlserver

import zio.schema.StandardType._
import zio.schema._
import zio.sql.driver.Renderer
import zio.sql.driver.Renderer.Extensions

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}

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
          buildWhereExpr(whereExpr)
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

    private def buildExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
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
        right.asInstanceOf[Expr[_, A, B]] match {
          case Expr.Literal(true)  => val _ = render("1")
          case Expr.Literal(false) => val _ = render("0")
          case otherValue          => buildExpr(otherValue)
        }
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

    /**
      * Drops the initial Litaral(true) present at the start of every WHERE expressions by default 
      * and proceeds to the rest of Expr's.
      */
    private def buildWhereExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
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

    private def buildInsertValues[A](col: Seq[A])(implicit render: Renderer, schema: Schema[A]): Unit =
      col.toList match {
        case head :: Nil  =>
          render("(")
          buildInsertValue(head)
          render(");")
        case head :: next =>
          render("(")
          buildInsertValue(head)(render, schema)
          render(" ),")
          buildInsertValues(next)
        case Nil          => ()
      }

    private def buildInsertValue[Z](z: Z)(implicit render: Renderer, schema: Schema[Z]): Unit =
      schema.toDynamic(z) match {
        case DynamicValue.Record(listMap) =>
          listMap.values.toList match {
            case head :: Nil  => buildDynamicValue(head)
            case head :: next =>
              buildDynamicValue(head)
              render(", ")
              buildDynamicValues(next)
            case Nil          => ()
          }
        case value                        => buildDynamicValue(value)
    }

    private def buildDynamicValues(dynValues: List[DynamicValue])(implicit render: Renderer): Unit =
      dynValues match {
        case head :: Nil  => buildDynamicValue(head)
        case head :: tail =>
          buildDynamicValue(head)
          render(", ")
          buildDynamicValues(tail)
        case Nil          => ()
      }

    // TODO render each type according to their specifics & test it
    private def buildDynamicValue(dynValue: DynamicValue)(implicit render: Renderer): Unit =
      dynValue match {
        case DynamicValue.Primitive(value, typeTag) =>
          // need to do this since StandardType is invariant in A
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
                case BoolType                                   =>
                  val b = value.asInstanceOf[Boolean]
                  if (b) {
                    render('1')
                  } else {
                    render('0')
                  }
                case DayOfWeekType                              => render(s"'${value}'")
                case FloatType                                  => render(value)
                case StandardType.DurationType                  => render(s"'${value}'")
              }
            case None    => ()
          }
        case DynamicValue.Tuple(left, right)        =>
          buildDynamicValue(left)
          render(", ")
          buildDynamicValue(right)
        case DynamicValue.SomeValue(value)          => buildDynamicValue(value)
        case DynamicValue.NoneValue                 => render("null")
        case _                                      => ()
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

    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]) = {
      render("INSERT INTO ")
      buildTable(insert.table)

      render(" (")
      buildColumnNames(insert.sources)
      render(") VALUES ")

      buildInsertValues(insert.values)
    }
  }
}
