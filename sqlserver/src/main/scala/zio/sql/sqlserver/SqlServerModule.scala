package zio.sql.sqlserver

import zio.sql.Jdbc

trait SqlServerModule extends Jdbc { self =>

  import self.ColumnSet._

  override type TableExtension[A] = SqlServerSpecific.SqlServerTable[A]

  object SqlServerSpecific {

    sealed trait SqlServerTable[A] extends Table.TableEx[A]

    object SqlServerTable {

      import scala.language.implicitConversions

      sealed trait CrossType
      object CrossType {
        case object CrossApply extends CrossType
        case object OuterApply extends CrossType
      }

      private[SqlServerModule] sealed case class CrossOuterApplyTable[A, B](
        crossType: CrossType,
        left: Table.Aux[A],
        right: Table.Aux[B]
      ) extends SqlServerTable[A with B] { self =>

        override type ColumnHead = left.ColumnHead
        override type ColumnTail =
          left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail]]

        override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail] = 
          left.columnSet ++ right.columnSet

        override val columnToExpr: ColumnToExpr[A with B] = new ColumnToExpr[A with B] {
          def toExpr[C](column: Column[C]): Expr[Features.Source, A with B, C] = 
            if (left.columnSet.contains(column))
              left.columnToExpr.toExpr(column)
            else
              right.columnToExpr.toExpr(column)
        }
      }

      implicit def tableSourceToSelectedBuilder[A](
        table: Table.Aux[A]
      ): CrossOuterApplyTableBuilder[A] =
        new CrossOuterApplyTableBuilder(table)

      sealed case class CrossOuterApplyTableBuilder[A](left: Table.Aux[A]) {
        self =>

        final def crossApply(
          right: Table.DerivedTable[Read[_]]
        ): Table.DialectSpecificTable[A with right.TableType] = {

          val tableExtension = CrossOuterApplyTable[A, right.TableType](
            CrossType.CrossApply,
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }

        final def outerApply(
          right: Table.DerivedTable[Read[_]]
        ): Table.DialectSpecificTable[A with right.TableType] = {

          val tableExtension = CrossOuterApplyTable[A, right.TableType](
            CrossType.OuterApply,
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }
      }
    }

    object QueriesExamples {

      val customers = (uuid("id") ++ string("first_name") ++ string("last_name")).table("customers")

      val customerId :*: fName :*: lName :*: _ = customers.columns

      val q = customers.columns

      val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

      val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns

      val orderDetails = (uuid("orderId") ++ uuid("product_id") ++ int("quantity") ++ double("unit_price")).table("order_details")

      val orderDetailsId :*: productId :*: quantity :*: unitPrice :*: _ = orderDetails.columns

      // ----------------

      val derived =
        select(customerId ++ fName).from(customers).asTable("derived")

     val derivedId :*: derivedName :*: _ = derived.columns

      //AS TABLE example
      val e = select(fName ++ lName).from(customers).asTable("derived")

      val ppp = select(orderId ++ fkCustomerId).from(orders).asTable("derived")

      val orders2 = select(orderDate).from(orders).asTable("derived")

      val wwq = select(fName ++ lName)
        .from(customers)

      val pok1 = customers.subselect(orderDate ++ fName).from(orders).where(customerId === fkCustomerId)

      val xwe22 = subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId)

      val pok         =
        subselectFrom(customers)(orderDate).from(orders).where(customerId === fkCustomerId).asTable("derived")
      val ordersTable =
        subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("derived")

      val orderDateColumn :*: _ = ordersTable.columns

      /*
                 select customers.id, customers.first_name, customers.last_name, ooo.order_date, ooo.id
                      from 
                        customers
                          cross apply (
                              select order_date
                              from orders
                              where orders.customer_id = customers.id
                          ) ooo
       */ 

      // Cross Apply example
     import SqlServerTable._


      /*TODO
        1. which one do we need ?
          * table.subquery(select)
          * subselect[TableType](select)
          * subselectFrom(parentTable)(query)
        2. rename those subqueries to correlated subqueries, add suppost for normal subqueries (ones which does not access parent table)
        3. add support for correlated subquery in where clause when accessing the same table
        4. correlated subqueries in selections /  where clauses
        5. translate DerivedTable also for postgres, Oracle, Mysql
        6. add test for outer apply and real cross apply
      */

      select(customerId ++ fName ++ lName)
        .from(customers
          .crossApply(
            subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("derived")
          )
        )

      val newOrdersTable = subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("derived")

      val localdate :*: _ = newOrdersTable.columns
      
      val crossApplyExample2 = select(customerId ++ fName ++ lName ++ localdate)
        .from(
          customers
            .crossApply(
              newOrdersTable
            )
        )

      val crossApplyExample3 = select(customerId ++ fName ++ lName)
        .from(
          customers
            .crossApply(
              subselectFrom(customers)(orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")
            )
        )

      val crossApplyExample4Alt = select(customerId ++ fName ++ lName)
        .from(
          customers
            .crossApply(
              customers.subselect(orderDate).from(orders).where(customerId === fkCustomerId).asTable("derived")
            )
        )

      val newtable = 
        customers
          .subselect(customerId ++ fName ++ lName ++ orderDate)
          .from(orders)
          .where(customerId === fkCustomerId)
          .asTable("")

      val dCustomerId :*: dfName :*: dflName :*: dOrderDate :*: _ = newtable.columns

      val crossApplyExample4 = select(dCustomerId ++ dfName ++ dflName ++ dOrderDate)
        .from(newtable)


      // //JOIN example
      val joinQuery = select(fName ++ lName ++ orderDate).from(customers.join(orders).on(customerId === fkCustomerId))

      // // SUBSELECT another example
      val xww  = subselect[customers.TableType](orderDate ++ fName).from(orders).where(customerId === fkCustomerId)
      val xwwx = subselect[customers.TableType](orderDate ++ fName)
        .from(orders)
        .where(customerId === fkCustomerId)
        .asTable("")

      val qqqqq =
        subselect[customers.TableType](orderDate).from(orders).where(customerId === fkCustomerId).asTable("ooo")


      //TODO
      // // ========= CORRELATED SUBQUERY

      // // select order_id, product_id, unit_price from order_details od
      // // where unit_price  > (select avg(unit_price) from order_details where od.product_id = product_id)

      // val customUUID = java.util.UUID.fromString("48ce2e6e-7258-413f-942f-01eb21acd979")

      // val re = select(AggregationDef.Avg(unitPrice)).from(orderDetails).where(productId === customUUID)
      
      // select(orderDetailsId ++ productId ++ unitPrice)
      //   .from(orderDetails)
      //   .where(unitPrice > select(AggregationDef.Avg(unitPrice)).from(orderDetails).where(productId === ???))
    }
  }

  override def renderDelete(delete: Delete[_]): String = ??? // TODO: https://github.com/zio/zio-sql/issues/159

  override def renderUpdate(update: self.Update[_]): String = ???

  override def renderRead(read: self.Read[_]): String = {
    val builder = new StringBuilder

    def buildExpr[A, B](expr: self.Expr[_, A, B]): Unit = expr match {
      case Expr.Subselect(subselect) => ???
      case Expr.Source(table, column)                                                           =>
        (table, column) match {
          case (tableName: TableName, Column.Named(columnName)) =>
            val _ = builder.append(tableName).append(".").append(columnName)
          case _                                                => ()
        }
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

        case Table.DerivedTable(read, name) => {
              builder.append(renderRead(read))

              builder.append(" ) ")
              val _ = builder.append(name)
        }
        
        case sourceTable: self.Table.Source    =>
          val _ = builder.append(sourceTable.name)

        case Table.DialectSpecificTable(tableExtension) =>
          tableExtension match {
            case SqlServerSpecific.SqlServerTable.CrossOuterApplyTable(crossType, left, derivedTable) => {
              buildTable(left)

              crossType match {
                case SqlServerSpecific.SqlServerTable.CrossType.CrossApply => builder.append(" cross apply ( ")
                case SqlServerSpecific.SqlServerTable.CrossType.OuterApply => builder.append(" outer apply ( ")
              }

              val _ = buildTable(derivedTable)
            }
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
