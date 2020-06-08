package zio.sql

trait Renderable {
  def render(mode: RenderMode): String = {
    val builder = new StringBuilder
    renderBuilder(builder, mode)
    builder.toString()
  }

  private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit
}
sealed trait RenderMode
object RenderMode {
  case object Compact extends RenderMode
  final case class Pretty(indent: Int) extends RenderMode
}
