package zio.sql.sqlserver

import zio.sql.Jdbc

import scala.language.implicitConversions

trait SqlServerModule extends Jdbc { self =>

  import self.ColumnSet._

  override type TableExtension[+A] = SqlServerSpecific.SqlServerTable[A]

  object SqlServerSpecific {

    sealed trait SqlServerTable[+A] extends Table.TableEx

    object SqlServerTable {

      sealed case class SelectedTable[F1, F2, ColumnsRepr[_], Cols, A, B](
        crossType: CrossType,
        left: Table.Source.Aux[ColumnsRepr, A, Cols],
        //tableValuedFuctnion : TableValuedFunction ???
        select: Read.Select[F1, Cols, B],
        expr: Expr[_, A with B, Boolean]
      ) extends SqlServerTable[A with B] { self =>

        // def where(whereExpr: Expr[F1 :||: F2, A with B, Boolean]): SelectedTable[F1, F2, ColumnsRepr, Cols, A, B] =
        //   self.copy(expr = whereExpr)

        def columnsUntyped: List[Column.Untyped] = left.columnsUntyped ++ select.table.get.columnsUntyped
      }

      implicit def tableSourceToSelectedBuilder[ColumnsRepr[_], A, Cols](
        table: Table.Source.Aux[ColumnsRepr, A, Cols]
      ): SelectedTableBuilder[ColumnsRepr, A, Cols] =
        new SelectedTableBuilder(table)

      sealed case class SelectedTableBuilder[ColumnsRepr[_], A, Cols](table: Table.Source.Aux[ColumnsRepr, A, Cols]) {
        self =>

        /**
          * Instead of Read.Select we need to accept some 'TableValuedFunction' which would: 
          * (or we could accept Select and turn it into TableValuedFunction - but we need to make sure only sensible things compile)
          *   - contain selection, table and optionally where clause
          *   - create new Table of type B created out of columns in select.selection
          *   - where clause need to access Table.Aux[A] and origin select.Table to create Expr.
          */
        final def crossApply[F1, F2, SelectionCols, SelectTableType](
          select: Read.Select[F1, SelectionCols, SelectTableType]
        ): SelectedTable[F1, F2, ColumnsRepr, Cols, table.TableType, SelectTableType] = {

          select.selection.value.selectionsUntyped.map(_.asInstanceOf[ColumnSelection[_, _]]).map {
            case t @ ColumnSelection.Constant(value, _) => t.typeTag
            case t @ ColumnSelection.Computed(expr, _)  => expr
          }
          ???
        }

        final def outerApply[F1, F2, SelectTableType](
          select: Read.Select[F1, Cols, SelectTableType]
        ): SelectedTable[F1, F2, ColumnsRepr, Cols, table.TableType, SelectTableType] =
          SelectedTable[F1, F2, ColumnsRepr, Cols, A, SelectTableType](
            CrossType.OuterApply,
            table,
            select,
            Expr.literal(false)
          )
      }
    }

    sealed trait CrossType
    object CrossType {
      case object CrossApply extends CrossType
      case object OuterApply extends CrossType
    }

    object QueriesExamples {

      val customers =
        (uuid("id") ++ string("first_name") ++ string("last_name") ++ boolean("verified") ++ localDate("dob"))
          .table("customers")

      val customerId :*: fName :*: lName :*: verified :*: dob :*: _ =
        customers.columns

      val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

      val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns

      //JOIN example
      val joinQuery = select(fName ++ lName ++ orderDate).from(customers.join(orders).on(customerId === fkCustomerId))
      
      // Cross Apply example
      // import SqlServerTable._
      // val crossApplyExample = select(fName ++ lName ++ orderDate ++ fkCustomerId).from(customers.crossApply(select(orderDate).from(orders).where(customerId === fkCustomerId)))


      val q = select(orderDate).from(orders).where(fkCustomerId === "")
    }
  }

  override def renderDelete(delete: Delete[_]): String = ??? // TODO: https://github.com/zio/zio-sql/issues/159

  override def renderUpdate(update: self.Update[_]): String = ???

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder

    def buildExpr[A, B](expr: self.Expr[_, A, B]): Unit = expr match {
      case Expr.Source(tableName, column)                                                       =>
        val _ = builder.append(tableName).append(".").append(column.name)
      case Expr.Unary(base, op)                                                                 =>
        val _ = builder.append(" ").append(op.symbol)
        buildExpr(base)
      case Expr.Property(base, op)                                                              =>
        buildExpr(base)
        val _ = builder.append(" ").append(op.symbol)
      case Expr.Binary(left, right, op)                                                         =>
        buildExpr(left)
        builder.append(" ").append(op.symbol).append(" ")
        buildExpr(right)
      case Expr.Relational(left, right, op)                                                     =>
        buildExpr(left)
        builder.append(" ").append(op.symbol).append(" ")
        buildExpr(right)
      case Expr.In(value, set)                                                                  =>
        buildExpr(value)
        buildReadString(set)
      case Expr.Literal(value)                                                                  =>
        val _ = builder.append(value.toString) //todo fix escaping
      case Expr.AggregationCall(param, aggregation)                                             =>
        builder.append(aggregation.name.name)
        builder.append("(")
        buildExpr(param)
        val _ = builder.append(")")
      case Expr.ParenlessFunctionCall0(function)                                                =>
        val _ = builder.append(function.name)
      case Expr.FunctionCall0(function)                                                         =>
        builder.append(function.name.name)
        builder.append("(")
        val _ = builder.append(")")
      case Expr.FunctionCall1(param, function)                                                  =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param)
        val _ = builder.append(")")
      case Expr.FunctionCall2(param1, param2, function)                                         =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        val _ = builder.append(")")
      case Expr.FunctionCall3(param1, param2, param3, function)                                 =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        val _ = builder.append(")")
      case Expr.FunctionCall4(param1, param2, param3, param4, function)                         =>
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
      case Expr.FunctionCall5(param1, param2, param3, param4, param5, function)                 =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        builder.append(",")
        buildExpr(param4)
        builder.append(",")
        buildExpr(param5)
        val _ = builder.append(")")
      case Expr.FunctionCall6(param1, param2, param3, param4, param5, param6, function)         =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        builder.append(",")
        buildExpr(param4)
        builder.append(",")
        buildExpr(param5)
        builder.append(",")
        buildExpr(param6)
        val _ = builder.append(")")
      case Expr.FunctionCall7(param1, param2, param3, param4, param5, param6, param7, function) =>
        builder.append(function.name.name)
        builder.append("(")
        buildExpr(param1)
        builder.append(",")
        buildExpr(param2)
        builder.append(",")
        buildExpr(param3)
        builder.append(",")
        buildExpr(param4)
        builder.append(",")
        buildExpr(param5)
        builder.append(",")
        buildExpr(param6)
        builder.append(",")
        buildExpr(param7)
        val _ = builder.append(")")
    }

    def buildReadString(read: self.Read[_]): Unit =
      read match {
        case Read.Mapped(read, _) => buildReadString(read)

        //todo offset (needs orderBy, must use fetch _instead_ of top)
        case read0 @ Read.Select(_, _, _, _, _, _, _, _) =>
          object Dummy {
            type F
            type A
            type B <: SelectionSet[A]
          }
          val read = read0.asInstanceOf[Read.Select[Dummy.F, Dummy.A, Dummy.B]]
          import read._

          builder.append("select ")
          limit match {
            case Some(limit) =>
              builder.append("top ").append(limit).append(" ")
            case None        => ()
          }
          buildSelection(selection.value)
          table.foreach { t =>
            builder.append(" from ")
            buildTable(t)
          }
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  =>
              builder.append(" where ")
              buildExpr(whereExpr)
          }
          groupBy match {
            case _ :: _ =>
              builder.append(" group by ")
              buildExprList(groupBy)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  builder.append(" having ")
                  buildExpr(havingExpr)
              }
            case Nil    => ()
          }
          orderBy match {
            case _ :: _ =>
              builder.append(" order by ")
              buildOrderingList(orderBy)
            case Nil    => ()
          }

        case Read.Union(left, right, distinct) =>
          buildReadString(left)
          builder.append(" union ")
          if (!distinct) builder.append("all ")
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
              builder.append(" desc")
          }
          tail match {
            case _ :: _ =>
              builder.append(", ")
              buildOrderingList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def buildSelection(selectionSet: SelectionSet[_]): Unit =
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
              val _ = builder.append(" as ").append(name)
            case None       => ()
          }
        case ColumnSelection.Computed(expr, name)  =>
          buildExpr(expr)
          name match {
            case Some(name) =>
              Expr.exprName(expr) match {
                case Some(sourceName) if name != sourceName =>
                  val _ = builder.append(" as ").append(name)
                case _                                      => ()
              }
            case _          => () //todo what do we do if we don't have a name?
          }
      }

    def buildTable(table: Table): Unit =
      table match {
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: self.Table.Source    =>
          val _ = builder.append(sourceTable.name)

        case Table.DialectSpecificTable(table) =>
          table match {
            case SqlServerSpecific.SqlServerTable.SelectedTable(crossType, left, select, on) =>
              buildTable(left)

              crossType match {
                case SqlServerSpecific.CrossType.CrossApply => builder.append(" CROSS APPLY ( ")
                case SqlServerSpecific.CrossType.OuterApply => builder.append(" OUTER APPLY ( ")
              }

              builder.append("SELECT ")
              buildSelection(select.selection.value)
              builder.append(" FROM ")
              buildTable(select.table.get)
              builder.append(" WHERE ")
              buildExpr(on)

              builder.append(" ) ")
              val _ = buildTable(select.table.get)

          }

        case Table.Joined(joinType, left, right, on) =>
          buildTable(left)
          builder.append(joinType match {
            case JoinType.Inner      => " inner join "
            case JoinType.LeftOuter  => " left join "
            case JoinType.RightOuter => " right join "
            case JoinType.FullOuter  => " outer join "
          })
          buildTable(right)
          builder.append(" on ")
          buildExpr(on)
          val _ = builder.append(" ")
      }
    buildReadString(read)
    builder.toString()
  }
}
