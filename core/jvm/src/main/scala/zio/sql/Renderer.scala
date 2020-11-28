package zio.sql

class Renderer(val builder: StringBuilder) extends AnyVal { self =>
  //not using vararg to avoid allocating `Seq`s
  def apply[A](a: A)(implicit rendering: Rendering[A]): Unit = rendering.rendering(a)(self)

  def apply[A, B](a: A, b: B)(implicit ra: Rendering[A], rb: Rendering[B]): Unit = {
    apply(a)
    apply(b)
  }

  def apply[A, B, C](a: A, b: B, c: C)(implicit
    ra: Rendering[A],
    rb: Rendering[B],
    rc: Rendering[C]
  ): Unit = {
    apply(a)
    apply(b)
    apply(c)
  }

  def apply[A, B, C, D](a: A, b: B, c: C, d: D)(implicit
    ra: Rendering[A],
    rb: Rendering[B],
    rc: Rendering[C],
    rd: Rendering[D]
  ): Unit = {
    apply(a)
    apply(b)
    apply(c)
    apply(d)
  }

  override def toString: String = builder.toString()

  def append(a: Any): Unit = {
    val _ = builder.append(a)
  }
}

object Renderer {
  def apply(): Renderer = new Renderer(new StringBuilder)
}

trait Rendering[-A] {
  def rendering(a: A)(implicit render: Renderer): Unit
}

object Rendering extends LowPriorityRenderingImplicits

abstract class LowPriorityRenderingImplicits {
  implicit case object DefaultRenderer extends Rendering[Any] {
    override def rendering(a: Any)(implicit render: Renderer): Unit = {
      val _ = render.append(a)
    }
  }
}
