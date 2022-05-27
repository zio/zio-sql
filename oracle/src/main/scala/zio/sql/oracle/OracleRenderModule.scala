package zio.sql.oracle

import zio.schema.Schema
import zio.schema.DynamicValue
import zio.schema.StandardType
import java.time.Instant
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import java.time.LocalTime
import java.time.OffsetTime
import java.time.LocalDate

trait OracleRenderModule extends OracleSqlModule { self =>

  override def renderDelete(delete: self.Delete[_]): String = {
    val builder = new StringBuilder
    buildDeleteString(delete, builder)
    builder.toString
  }

  override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = {
    val builder = new StringBuilder
    buildInsertString(insert, builder)
    builder.toString()
  }

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder
    buildReadString(read, builder)
    builder.toString()
  }

  override def renderUpdate(update: self.Update[_]): String = ???

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

  private def buildInsertString[A: Schema](insert: self.Insert[_, A], builder: StringBuilder): Unit = {

    builder.append("INSERT INTO ")
    renderTable(insert.table, builder)

    builder.append(" (")
    renderColumnNames(insert.sources, builder)
    builder.append(") VALUES ")

    renderInsertValues(insert.values, builder)
  }

  private def renderTable(table: Table, builder: StringBuilder): Unit                                         = table match {
    case Table.DerivedTable(read, name)             =>
      builder.append(" ( ")
      builder.append(renderRead(read.asInstanceOf[Read[_]]))
      builder.append(" ) ")
      builder.append(name)
      ()
    case Table.DialectSpecificTable(tableExtension) => ???
    case Table.Joined(joinType, left, right, on)    =>
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
    case source: Table.Source                       =>
      builder.append(quoted(source.name))
      ()
  }
  private def renderColumnNames(sources: SelectionSet[_], builder: StringBuilder): Unit                       =
    sources match {
      case SelectionSet.Empty                       => () // table is a collection of at least ONE column
      case SelectionSet.Cons(columnSelection, tail) =>
        val _ = columnSelection.name.map { name =>
          builder.append(quoted(name))
        }
        tail.asInstanceOf[SelectionSet[_]] match {
          case SelectionSet.Empty             => ()
          case next @ SelectionSet.Cons(_, _) =>
            builder.append(", ")
            renderColumnNames(next.asInstanceOf[SelectionSet[_]], builder)
        }
    }
  private def renderInsertValues[A](values: Seq[A], builder: StringBuilder)(implicit schema: Schema[A]): Unit =
    values.toList match {
      case head :: Nil  =>
        builder.append("(")
        renderInsertValue(head, builder)
        builder.append(");")
        ()
      case head :: next =>
        builder.append("(")
        renderInsertValue(head, builder)
        builder.append(" ),")
        renderInsertValues(next, builder)
      case Nil          => ()
    }

  def renderInsertValue[Z](z: Z, builder: StringBuilder)(implicit schema: Schema[Z]): Unit =
    schema.toDynamic(z) match {
      case DynamicValue.Record(listMap) =>
        listMap.values.toList match {
          case head :: Nil  => renderDynamicValue(head, builder)
          case head :: next =>
            renderDynamicValue(head, builder)
            builder.append(", ")
            renderDynamicValues(next, builder)
          case Nil          => ()
        }
      case value                        => renderDynamicValue(value, builder)
    }

  def renderDynamicValue(dynValue: DynamicValue, builder: StringBuilder): Unit =
    dynValue match {
      case DynamicValue.Primitive(value, typeTag) =>
        // need to do this since StandardType is invariant in A
        import StandardType._
        StandardType.fromString(typeTag.tag) match {
          case Some(v) =>
            v match {
              case BigDecimalType                             =>
                builder.append(value)
                ()
              case StandardType.InstantType(formatter)        =>
                builder.append(s"'${formatter.format(value.asInstanceOf[Instant])}'")
                ()
              case CharType                                   =>
                builder.append(s"'${value}'")
                ()
              case IntType                                    =>
                builder.append(value)
                ()
              case StandardType.MonthDayType                  =>
                builder.append(s"'${value}'")
                ()
              case BinaryType                                 =>
                builder.append(s"'${value}'")
                ()
              case StandardType.MonthType                     =>
                builder.append(s"'${value}'")
                ()
              case StandardType.LocalDateTimeType(formatter)  =>
                builder.append(s"'${formatter.format(value.asInstanceOf[LocalDateTime])}'")
                ()
              case UnitType                                   =>
                builder.append("null") // None is encoded as Schema[Unit].transform(_ => None, _ => ())
                ()
              case StandardType.YearMonthType                 =>
                builder.append(s"'${value}'")
                ()
              case DoubleType                                 =>
                builder.append(value)
                ()
              case StandardType.YearType                      =>
                builder.append(s"'${value}'")
                ()
              case StandardType.OffsetDateTimeType(formatter) =>
                builder.append(s"'${formatter.format(value.asInstanceOf[OffsetDateTime])}'")
                ()
              case StandardType.ZonedDateTimeType(_)          =>
                builder.append(s"'${DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[ZonedDateTime])}'")
                ()
              case BigIntegerType                             =>
                builder.append(s"'${value}'")
                ()
              case UUIDType                                   =>
                builder.append(s"'${value}'")
                ()
              case StandardType.ZoneOffsetType                =>
                builder.append(s"'${value}'")
                ()
              case ShortType                                  =>
                builder.append(value)
                ()
              case StandardType.LocalTimeType(formatter)      =>
                builder.append(s"'${formatter.format(value.asInstanceOf[LocalTime])}'")
                ()
              case StandardType.OffsetTimeType(formatter)     =>
                builder.append(s"'${formatter.format(value.asInstanceOf[OffsetTime])}'")
                ()
              case LongType                                   =>
                builder.append(value)
                ()
              case StringType                                 =>
                builder.append(s"'${value}'")
                ()
              case StandardType.PeriodType                    =>
                builder.append(s"'${value}'")
                ()
              case StandardType.ZoneIdType                    =>
                builder.append(s"'${value}'")
                ()
              case StandardType.LocalDateType(formatter)      =>
                builder.append(s"'${formatter.format(value.asInstanceOf[LocalDate])}'")
                ()
              case BoolType                                   =>
                builder.append(value)
                ()
              case DayOfWeekType                              =>
                builder.append(s"'${value}'")
                ()
              case FloatType                                  =>
                builder.append(value)
                ()
              case StandardType.DurationType                  =>
                builder.append(s"'${value}'")
                ()
            }
          case None    => ()
        }
      case DynamicValue.Tuple(left, right)        =>
        renderDynamicValue(left, builder)
        builder.append(", ")
        renderDynamicValue(right, builder)
      case DynamicValue.SomeValue(value)          => renderDynamicValue(value, builder)
      case DynamicValue.NoneValue                 =>
        builder.append("null")
        ()
      case _                                      => ()
    }

  def renderDynamicValues(dynValues: List[DynamicValue], builder: StringBuilder): Unit =
    dynValues match {
      case head :: Nil  => renderDynamicValue(head, builder)
      case head :: tail =>
        renderDynamicValue(head, builder)
        builder.append(", ")
        renderDynamicValues(tail, builder)
      case Nil          => ()
    }

  private def quoted(name: String): String = "\"" + name + "\""

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

}
