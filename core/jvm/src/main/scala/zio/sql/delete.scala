package zio.sql

trait DeleteModule { self: ExprModule with TableModule =>

  sealed case class DeleteBuilder[F[_], A, B](table: Table.Aux[A]) {
    def where[F1](expr: Expr[F1, A, Boolean]): Delete[F1, A] = Delete(table, expr)
  }

  sealed case class Delete[F, A](
    table: Table.Aux[A],
    whereExpr: Expr[F, A, Boolean]
  ) extends Renderable {
    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      builder.append("delete from ")
      table.renderBuilder(builder, mode)
      /*todo check whereExpr
        whereExpr match {
        case Expr.Literal(true) => ()
        case _ =>*/
      builder.append(" where ")
      whereExpr.renderBuilder(builder, mode)
      //}
    }
  }

}