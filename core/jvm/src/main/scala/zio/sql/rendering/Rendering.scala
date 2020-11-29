package zio.sql.rendering

trait Rendering[-A] {
  def apply(a: A)(implicit builder: Builder): Unit
}
object Rendering extends LowPriorityRenderingImplicits

abstract class LowPriorityRenderingImplicits {
  //todo add constants for common characters e.g. "'"
  implicit case object DefaultRenderer extends Rendering[String] {
    override def apply(a: String)(implicit builder: Builder): Unit = {
      //todo escaping
      val _ = builder.append(a)
    }
  }
}
