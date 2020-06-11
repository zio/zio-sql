package zio.sql

trait Renderable {
  def render(mode: RenderMode): String = {
    val builder = new StringBuilder
    renderBuilder(builder, mode)
    builder.toString()
  }

  private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit
}
object Renderable {
  implicit class ListOps(val list: List[Renderable]) extends Renderable {
    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit =
      list match {
        case head :: tail =>
          head.renderBuilder(builder, mode)
          tail match {
            case _ :: _ => builder.append(", ")
            case Nil    => ()
          }
          tail.renderBuilder(builder, mode)
        case Nil => ()
      }
  }
}

sealed trait RenderMode
object RenderMode {
  case object Compact                  extends RenderMode
  final case class Pretty(indent: Int) extends RenderMode
}
