package zio.sql

import zio.sql.sqlserver.SqlServerModule
import zio.schema._
import java.time._
import java.util.UUID

object Examples extends App with SqlServerModule {
  import this.AggregationDef._
  import this.FunctionDef._
  import Orders._
  import Users._
  import OrderDetails._

  val basicSelect =
    select(fName, lName).from(users)

  println(renderRead(basicSelect))

  val basicSelectWithAliases =
    select((fName as "first"), (lName as "last")).from(users)
  println(renderRead(basicSelectWithAliases))

  val selectWithRefinements =
    select(fName, lName)
      .from(users)
      .orderBy(lName, fName.desc)
      .limit(2)
  println(renderRead(selectWithRefinements))

  val basicDelete =
    deleteFrom(users).where(fName === "Terrence")
  println(renderDelete(basicDelete))

  val basicJoin =
    select(fName, lName, orderDate).from(users.leftOuter(orders).on(fkUserId === userId))
  println(renderRead(basicJoin))

  val orderValues =
    select(
      userId,
      fName,
      lName,
      (Sum(quantity * unitPrice) as "total_spend"),
      Sum(Abs(quantity))
    )
      .from(
        users
          .join(orders)
          .on(userId === fkUserId)
          .leftOuter(orderDetails)
          .on(orderId === fkOrderId)
      )
      .groupBy(userId, fName, lName)
  println(renderRead(orderValues))

  import scala.language.postfixOps

  val withPropertyOp = select(fName, lName).from(users).where(fName isNotNull)
  println(renderRead(withPropertyOp))

  val exx = orderId ++ fkUserId

  val query = select(fkUserId, Count(orderId))
    .from(orders)
    .groupBy(fkUserId, orderDate)

  val e = select(Count(orderId), Count(orderId))
    .from(orders)

  // val ee: Selection[Features.Source["customer_id",orders.TableType] with Features.Aggregated[Features.Source["id",orders.TableType]],
  //   orders.TableType,SelectionSet.Cons[orders.TableType,java.util.UUID,SelectionSet.Cons[orders.TableType,Long,SelectionSet.Empty]]] = ???

  // val ee1: Selection[Features.Source["customer_id",orders.TableType] with Features.Source["composite",orders.TableType] with Features.Aggregated[Features.Source["id",orders.TableType]],
  //   orders.TableType,SelectionSet.Cons[orders.TableType,java.util.UUID,SelectionSet.Cons[orders.TableType,Long,SelectionSet.Empty]]] = ???

  // val eee: Selection[Features.Aggregated[Features.Source["id",orders.TableType]] with Features.Source["customer_id",orders.TableType],
  //   orders.TableType,SelectionSet.Cons[orders.TableType,java.util.UUID,SelectionSet.Cons[orders.TableType,Long,SelectionSet.Empty]]] = ???

  // val eee2: Selection[Features.Source["customer_id",orders.TableType] with Features.Aggregated[Features.Source["id",orders.TableType]] with Features.Source["composite",orders.TableType],
  //   orders.TableType,SelectionSet.Cons[orders.TableType,java.util.UUID,SelectionSet.Cons[orders.TableType,Long,SelectionSet.Empty]]] = ???

  // val eee3: Selection[Features.Aggregated[Features.Source["id",orders.TableType]] with Features.Source["composite",orders.TableType] with Features.Source["customer_id",orders.TableType] ,
  //   orders.TableType,SelectionSet.Cons[orders.TableType,java.util.UUID,SelectionSet.Cons[orders.TableType,Long,SelectionSet.Empty]]] = ???

  // val q = select(fkUserId, Count(orderId))

  // Features.Source["customer_id",orders.TableType] with Features.Aggregated[Features.Source["id",orders.TableType]]
  def test[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B])(implicit
    i: IsPartiallyAggregated2[F]
  ): i.Unaggregated = ???

  // val y = test(ee)

  /**
    * Diverging implicit expansion,
    * 
    * would it be possible to write a macro that would remove some specific type from intersection type? 
    * (A with B with C) - C = A with B
    */
  sealed trait IsPartiallyAggregated2[A] {
    type Unaggregated
  }

  object IsPartiallyAggregated2 extends IsPartiallyAggregated2Lower {

    type WithRemainder[F, R] = IsPartiallyAggregated2[F] {
      type Unaggregated = R
    }

    implicit def AggregatedIsAggregated[A]: IsPartiallyAggregated2.WithRemainder[Features.Aggregated[A], Any] =
      new IsPartiallyAggregated2[Features.Aggregated[A]] {
        override type Unaggregated = Any
      }

    implicit def LeftWithAggregated[ColumnName, TableType, A, Left, Right](implicit
      left: IsPartiallyAggregated2[Left],
      right: IsPartiallyAggregated2[Right]
    ): IsPartiallyAggregated2.WithRemainder[
      Left with Right,
      left.Unaggregated with right.Unaggregated
    ] =
      new IsPartiallyAggregated2[Left with Right] {
        override type Unaggregated = left.Unaggregated with right.Unaggregated
      }

  }
  trait IsPartiallyAggregated2Lower {

    implicit def SourceIsAggregated[ColumnName, TableType]: IsPartiallyAggregated2.WithRemainder[
      Features.Source[ColumnName, TableType],
      Features.Source[ColumnName, TableType]
    ] =
      new IsPartiallyAggregated2[Features.Source[ColumnName, TableType]] {
        override type Unaggregated = Features.Source[ColumnName, TableType]
      }
  }

  object Users {

    case class Users(id: UUID, dob: LocalDate, firstName: String, lastName: String)

    implicit val userSchema = DeriveSchema.gen[Users]

    val users = defineTable[Users]

    val (userId, dob, fName, lName) = users.columns
  }

  object Orders {

    case class Orders(id: java.util.UUID, userId: java.util.UUID, orderDate: LocalDate)

    implicit val orderSchema = DeriveSchema.gen[Orders]

    val orders = defineTable[Orders]

    val (orderId, fkUserId, orderDate) = orders.columns
  }

  object OrderDetails {
    case class OrderDetail(orderId: Int, productId: Int, quantity: Double, unitPrice: Double)

    implicit val orderDetailSchema = DeriveSchema.gen[OrderDetail]

    val orderDetails = defineTable[OrderDetail]

    val (fkOrderId, fkProductId, quantity, unitPrice) = orderDetails.columns
  }

}
