package zio.sql.oracle

import java.time._
import java.time.format.{ DateTimeFormatter, DateTimeFormatterBuilder }
import java.time.temporal.ChronoField._

import zio.schema.Schema
import zio.schema.DynamicValue
import zio.schema.StandardType
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

import scala.collection.mutable

trait OracleRenderModule extends OracleSqlModule { self =>

  override def renderDelete(delete: Delete[_]): String = {
    val builder = new StringBuilder
    buildDeleteString(delete, builder)
    builder.toString
  }

  override def renderInsert[A: Schema](insert: Insert[_, A]): SqlStatement = {
    implicit val render: Renderer = Renderer()
    val rows                      = OracleRender.renderInsertImpl(insert)
    SqlStatement(render.toString, rows)
  }

  override def renderRead(read: Read[_]): String = {
    val builder = new StringBuilder
    buildReadString(read, builder)
    builder.toString()
  }

  override def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    OracleRender.renderUpdateImpl(update)
    render.toString
  }

  private object DateTimeFormats {
    val fmtTime = new DateTimeFormatterBuilder()
      .appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)
      .appendLiteral(':')
      .appendValue(SECOND_OF_MINUTE, 2)
      .appendFraction(NANO_OF_SECOND, 9, 9, true)
      .appendOffset("+HH:MM", "Z")
      .toFormatter()

    val fmtTimeOffset = new DateTimeFormatterBuilder()
      .append(fmtTime)
      .appendFraction(NANO_OF_SECOND, 9, 9, true)
      .toFormatter()

    val fmtDateTime = new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral('T')
      .append(fmtTime)
      .toFormatter()

    val fmtDateTimeOffset = new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(fmtDateTime)
      .appendOffset("+HH:MM", "Z")
      .toFormatter()
  }

  private def buildLit(lit: Expr.Literal[_])(builder: StringBuilder): Unit = {
    import TypeTag._
    val value = lit.value
    lit.typeTag match {
      case TInstant        =>
        val _ = builder.append(s"""TO_TIMESTAMP_TZ('${DateTimeFormats.fmtDateTimeOffset.format(
            value.asInstanceOf[Instant]
          )}', 'SYYYY-MM-DD"T"HH24:MI:SS.FF9TZH:TZM')""")
      case TLocalTime      =>
        val localTime = value.asInstanceOf[LocalTime]
        val _         = builder.append(
          s"INTERVAL '${localTime.getHour}:${localTime.getMinute}:${localTime.getSecond}.${localTime.getNano}' HOUR TO SECOND(9)"
        )
      case TLocalDate      =>
        val _ = builder.append(
          s"TO_DATE('${DateTimeFormatter.ISO_LOCAL_DATE.format(value.asInstanceOf[LocalDate])}', 'SYYYY-MM-DD')"
        )
      case TLocalDateTime  =>
        val _ = builder.append(s"""TO_TIMESTAMP('${DateTimeFormats.fmtDateTime.format(
            value.asInstanceOf[LocalDateTime]
          )}', 'SYYYY-MM-DD"T"HH24:MI:SS.FF9')""")
      case TZonedDateTime  =>
        val _ = builder.append(s"""TO_TIMESTAMP_TZ('${DateTimeFormats.fmtDateTimeOffset.format(
            value.asInstanceOf[ZonedDateTime]
          )}', 'SYYYY-MM-DD"T"HH24:MI:SS.FF9TZH:TZM')""")
      case TOffsetTime     =>
        val _ = builder.append(
          s"TO_TIMESTAMP_TZ('${DateTimeFormats.fmtTimeOffset.format(value.asInstanceOf[OffsetTime])}', 'HH24:MI:SS.FF9TZH:TZM')"
        )
      case TOffsetDateTime =>
        val _ = builder.append(
          s"""TO_TIMESTAMP_TZ('${DateTimeFormats.fmtDateTimeOffset.format(
              value.asInstanceOf[OffsetDateTime]
            )}', 'SYYYY-MM-DD"T"HH24:MI:SS.FF9TZH:TZM')"""
        )

      case TBoolean =>
        val b = value.asInstanceOf[Boolean]
        if (b) {
          val _ = builder.append('1')
        } else {
          val _ = builder.append('0')
        }
      case TUUID    =>
        val _ = builder.append(s"'$value'")

      case TBigDecimal =>
        val _ = builder.append(value)
      case TByte       =>
        val _ = builder.append(value)
      case TDouble     =>
        val _ = builder.append(value)
      case TFloat      =>
        val _ = builder.append(value)
      case TInt        =>
        val _ = builder.append(value)
      case TLong       =>
        val _ = builder.append(value)
      case TShort      =>
        val _ = builder.append(value)

      case TChar   =>
        val _ = builder.append(s"N'$value'")
      case TString =>
        val _ = builder.append(s"N'$value'")

      case _ =>
        val _ = builder.append(s"'$value'")
    }
  }

  // TODO: to consider the refactoring and using the implicit `Renderer`, see `renderExpr` in `PostgresRenderModule`
  private def buildExpr[A, B](expr: Expr[_, A, B], builder: StringBuilder): Unit = expr match {
    case Expr.Subselect(subselect)                                                            =>
      builder.append(" (")
      builder.append(renderRead(subselect))
      val _ = builder.append(") ")
    case Expr.Source(table, column)                                                           =>
      (table, column.name) match {
        case (tableName: String, Some(columnName)) =>
          val _ = builder.append(tableName).append(".").append(columnName)
        case _                                     => ()
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
      right.asInstanceOf[Expr[_, A, B]] match {
        case Expr.Literal(true)  => val _ = builder.append("1")
        case Expr.Literal(false) => val _ = builder.append("0")
        case otherValue          => buildExpr(otherValue, builder)
      }
    case Expr.In(value, set)                                                                  =>
      buildExpr(value, builder)
      buildReadString(set, builder)
    case Expr.Literal(true)                                                                   =>
      val _ = builder.append("1 = 1")
    case Expr.Literal(false)                                                                  =>
      val _ = builder.append("0 = 1")
    case literal: Expr.Literal[_]                                                             =>
      val _ = buildLit(literal)(builder)
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

  /**
    * Drops the initial Litaral(true) present at the start of every WHERE expressions by default
    * and proceeds to the rest of Expr's.
    */
  private def buildWhereExpr[A, B](expr: Expr[_, A, B], builder: mutable.StringBuilder): Unit = expr match {
    case Expr.Literal(true)   => ()
    case Expr.Binary(_, b, _) =>
      builder.append(" WHERE ")
      buildExpr(b, builder)
    case _                    =>
      builder.append(" WHERE ")
      buildExpr(expr, builder)
  }

  private def buildReadString(read: Read[_], builder: StringBuilder): Unit =
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
        buildWhereExpr(whereExpr, builder)
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

  private def renderTable(table: Table, builder: StringBuilder): Unit = table match {
    case Table.DerivedTable(read, name)          =>
      builder.append(" ( ")
      builder.append(renderRead(read.asInstanceOf[Read[_]]))
      builder.append(" ) ")
      builder.append(name)
      ()
    case Table.DialectSpecificTable(_)           => ??? // there are no extensions for Oracle
    case Table.Joined(joinType, left, right, on) =>
      renderTable(left, builder)
      val joinTypeRepr = joinType match {
        case JoinType.Inner      => " INNER JOIN "
        case JoinType.LeftOuter  => " LEFT JOIN "
        case JoinType.RightOuter => " RIGHT JOIN "
        case JoinType.FullOuter  => " OUTER JOIN "
      }
      builder.append(joinTypeRepr)
      renderTable(right, builder)
      builder.append(" ON ")
      buildExpr(on, builder)
      builder.append(" ")
      ()
    case source: Table.Source                    =>
      builder.append(source.name)
      ()
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
      case sourceTable: Table.Source               =>
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

  private def buildDeleteString(delete: Delete[_], builder: mutable.StringBuilder): Unit = {
    builder.append("DELETE FROM ")
    buildTable(delete.table, builder)
    buildWhereExpr(delete.whereExpr, builder)
  }

  private[oracle] object OracleRender {

    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]): List[SqlRow] = {

      render("INSERT INTO ")
      renderTable(insert.table, render.builder)

      render(" (")
      renderColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertPlaceholders(insert.values)

      buildInsertList(insert.values)
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
      // TODO any performance penalty because of toList ?
      col.toList match {
        case head :: _ =>
          render("(")
          renderInsertPlaceholder(head)
          render(")")
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

    private def renderColumnNames(sources: SelectionSet[_])(implicit render: Renderer): Unit =
      sources match {
        case SelectionSet.Empty                       => () // table is a collection of at least ONE column
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

    def renderUpdateImpl(update: Update[_])(implicit render: Renderer): Unit =
      update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ")
          buildTable(table, render.builder)
          render(" SET ")
          renderSet(set)
          buildWhereExpr(whereExpr, render.builder)
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

    private[zio] def renderSetLhs[A, B](expr: Expr[_, A, B])(implicit render: Renderer): Unit =
      expr match {
        case Expr.Source(table, column) =>
          (table, column.name) match {
            case (tableName, Some(columnName)) => val _ = render(tableName, ".", columnName)
            case _                             => ()
          }
        case _                          => ()
      }
  }
}
