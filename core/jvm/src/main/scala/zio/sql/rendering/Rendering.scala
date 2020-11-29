package zio.sql.rendering

trait Rendering[-A] {
  def apply(a: A)(implicit builder: Builder): Unit
}

object Rendering { //extends LowPriorityRenderingImplicits todo remove?
  implicit case object DefaultRenderer extends Rendering[String] {
    override def apply(a: String)(implicit builder: Builder): Unit = {
      //todo escaping
      val _ = builder.append(a)
    }
  }
}
/*
abstract class LowPriorityRenderingImplicits {
  implicit case object DefaultRenderer extends Rendering[String] {
    override def apply(a: String)(implicit builder: Builder): Unit = {
      //todo escaping
      val _ = builder.append(a)
    }
  }
}
 */
