package zio.sql.rendering

trait Rendering[-A] {
  def apply(a: A)(implicit builder: Builder): Unit
}
object Rendering    {
  implicit case object DefaultRenderer extends Rendering[String] {
    //todo add constants for common characters e.g. "'"
    override def apply(a: String)(implicit builder: Builder): Unit = {
      //todo escaping
      val _ = builder.append(a)
    }
  }
}

class Unescaped(val unescaped: String) extends AnyVal

abstract class LowPriorityRendering {}
