package zio.sql.postgresql

import zio.sql.Jdbc

import scala.language.implicitConversions

/**
 */
trait PostgresModule extends Jdbc { self =>
  override type ExprExtensionType[F, -A, B] = PostgresExprExtension[F, A, B]

  sealed trait PostgresExprExtension[F, -A, B]

  object PostgresExprExtension {
    implicit def postgresExpr2Expr[F, A, B: TypeTag](pgExpr: PostgresExprExtension[F, A, B]) =
      Expr.ExprDialectSpecific(pgExpr)

    implicit def postgresExpr2Selection[F, A, B: TypeTag](
      pgExpr: PostgresExprExtension[F, A, B]
    ): Selection[F, A, SelectionSet.Cons[A, B, SelectionSet.Empty]] =
      Selection.computedOption(pgExpr, Expr.exprName(pgExpr))

    sealed case class Overlay[F1, F2, F3, F4, Source, String](
      main: Expr[F1, Source, String],
      replacing: Expr[F2, Source, String],
      starting: Expr[F3, Source, Int],
      number: Expr[F4, Source, Int]
    ) extends PostgresExprExtension[F1 :||: F2 :||: F3 :||: F4, Source, String] {
      val functionName = "overlay"
    }
  }
  object PostgresFunctionDef   {
    val Sind    = FunctionDef[Double, Double](FunctionName("sind"))
    val Overlay = PostgresExprExtension.Overlay
  }

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder

    def buildExpr[A, B](expr: self.Expr[_, A, B]): Unit = expr match {
      case Expr.Source(tableName, column)                               =>
        val _ = builder.append(tableName).append(".").append(column.name)
      case Expr.Unary(base, op)                                         =>
        val _ = builder.append(" ").append(op.symbol)
        buildExpr(base)
      case Expr.Property(base, op)                                      =>
        buildExpr(base)
        val _ = builder.append(" ").append(op.symbol)
      case Expr.Binary(left, right, op)                                 =>
        buildExpr(left)
        builder.append(" ").append(op.symbol).append(" ")
        buildExpr(right)
      case Expr.Relational(left, right, op)                             =>
        buildExpr(left)
        builder.append(" ").append(op.symbol).append(" ")
        buildExpr(right)
      case Expr.In(value, set)                                          =>
        buildExpr(value)
        buildReadString(set)
      case Expr.Literal(value)                                          =>
        /*
        found   : PostgresModule.this.TypeTag.TZonedDateTime.type
[error]  required: PostgresModule.this.TypeTag[Any]
[error] Note: java.time.ZonedDateTime <: Any (and PostgresModule.this.TypeTag.TZonedDateTime.type <: PostgresModule.this.TypeTag.NotNull[java.time.ZonedDateTime]), but trait TypeTag is invariant in type A.

        import self.TypeTag._
        lit.typeTag match {
          //case tt @ TUUID => tt.cast(value) //why doesn't this work? it's still `Any`
          case TBigDecimal     => val _ = builder.append(value)
          case TBoolean        => val _ = builder.append(value)
          case TByte           => val _ = builder.append(value)
          case TByteArray      => val _ = builder.append(value)
          case TChar           => val _ = builder.append(value)
          case TDouble         => val _ = builder.append(value)
          case TFloat          => val _ = builder.append(value)
          case TInstant        => val _ = builder.append(value)
          case TInt            => val _ = builder.append(value)
          case TLocalDate      => val _ = builder.append(value)
          case TLocalDateTime  => val _ = builder.append(value)
          case TLocalTime      => val _ = builder.append(value)
          case TLong           => val _ = builder.append(value)
          case TOffsetDateTime => val _ = builder.append(value)
          case TOffsetTime     => val _ = builder.append(value)
          case TShort          => val _ = builder.append(value)
          case TString         =>
            builder.append("'")
            builder.append(value) //todo escape
            val _ = builder.append("'")
          case TUUID           => val _ = builder.append(value)
          case TZonedDateTime  => val _ = builder.append(value)
        }*/
        val _ = builder.append(value.toString) //todo fix escaping
      case Expr.AggregationCall(param, aggregation)                     =>
        builder.append(aggregation.name.name)
        builder.append("(")
        buildExpr(param)
        val _ = builder.append(")")
      case Expr.FunctionCall1(param, function)                          =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param)
        val _ = builder.append(")")
      case Expr.FunctionCall2(param1, param2, function)                 =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        val _ = builder.append(")")
      case Expr.FunctionCall3(param1, param2, param3, function)         =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        val _ = builder.append(")")
      case Expr.FunctionCall4(param1, param2, param3, param4, function) =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        builder.append(",")
        buildExpr(param4)
        val _ = builder.append(")")

      case Expr.ExprDialectSpecific(expr)                               =>
        expr match {
          case PostgresExprExtension.Overlay(main, replacing, starting, number) =>
            builder.append("overlay(")
            buildExpr(main)
            builder.append(" placing ")
            buildExpr(replacing)
            builder.append(" from ")
            buildExpr(starting)
            builder.append(" for ")
            buildExpr(number)
            val _ = builder.append(")")
        }
    }

    def buildReadString[A <: SelectionSet[_]](read: self.Read[_]): Unit =
      read match {
        case read0 @ Read.Select(_, _, _, _, _, _, _, _) =>
          object Dummy {
            type F
            type A
            type B <: SelectionSet[A]
          }
          val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.A, Dummy.B]]
          import read._

          builder.append("SELECT ")
          buildSelection(selection.value)
          builder.append(" FROM ")
          buildTable(table)
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  =>
              builder.append(" WHERE ")
              buildExpr(whereExpr)
          }
          groupBy match {
            case _ :: _ =>
              builder.append(" GROUP BY ")
              buildExprList(groupBy)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  builder.append(" HAVING ")
                  buildExpr(havingExpr)
              }
            case Nil    => ()
          }
          orderBy match {
            case _ :: _ =>
              builder.append(" ORDER BY ")
              buildOrderingList(orderBy)
            case Nil    => ()
          }
          limit match {
            case Some(limit) =>
              builder.append(" LIMIT ").append(limit)
            case None        => ()
          }
          offset match {
            case Some(offset) =>
              val _ = builder.append(" OFFSET ").append(offset)
            case None         => ()
          }

        case Read.Union(left, right, distinct) =>
          buildReadString(left)
          builder.append(" UNION ")
          if (!distinct) builder.append("ALL ")
          buildReadString(right)

        case Read.Literal(values) =>
          val _ = builder.append(" (").append(values.mkString(",")).append(") ") //todo fix needs escaping
      }

    def buildExprList(expr: List[Expr[_, _, _]]): Unit               =
      expr match {
        case head :: tail =>
          buildExpr(head)
          tail match {
            case _ :: _ =>
              builder.append(", ")
              buildExprList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }
    def buildOrderingList(expr: List[Ordering[Expr[_, _, _]]]): Unit =
      expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  => buildExpr(value)
            case Ordering.Desc(value) =>
              buildExpr(value)
              builder.append(" DESC")
          }
          tail match {
            case _ :: _ =>
              builder.append(", ")
              buildOrderingList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def buildSelection[A](selectionSet: SelectionSet[A]): Unit =
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
            builder.append(", ")
            buildSelection(tail)
          }
        case SelectionSet.Empty              => ()
      }

    def buildColumnSelection[A, B](columnSelection: ColumnSelection[A, B]): Unit =
      columnSelection match {
        case ColumnSelection.Constant(value, name) =>
          builder.append(value.toString()) //todo fix escaping
          name match {
            case Some(name) =>
              val _ = builder.append(" AS ").append(name)
            case None       => ()
          }
        case ColumnSelection.Computed(expr, name)  =>
          buildExpr(expr)
          name match {
            case Some(name) =>
              Expr.exprName(expr) match {
                case Some(sourceName) if name != sourceName =>
                  val _ = builder.append(" AS ").append(name)
                case _                                      => ()
              }
            case _          => () //todo what do we do if we don't have a name?
          }
      }
    def buildTable(table: Table): Unit                                           =
      table match {
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: self.Table.Source          =>
          val _ = builder.append(sourceTable.name)
        case Table.Joined(joinType, left, right, on) =>
          buildTable(left)
          builder.append(joinType match {
            case JoinType.Inner      => " INNER JOIN "
            case JoinType.LeftOuter  => " LEFT JOIN "
            case JoinType.RightOuter => " RIGHT JOIN "
            case JoinType.FullOuter  => " OUTER JOIN "
          })
          buildTable(right)
          builder.append(" ON ")
          buildExpr(on)
          val _ = builder.append(" ")
      }
    buildReadString(read)
    builder.toString()
  }
}
