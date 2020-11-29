package zio.sql.rendering

trait PlatformSpecificRenderer[R[-A] <: Rendering[A]] {
  //not using vararg to avoid allocating `Seq`s
  def apply[A](a: A)(implicit r: R[A], builder: Builder): Unit = r.apply(a)(builder)

  def apply[A, B](a: A, b: B)(implicit ra: R[A], rb: R[B], builder: Builder): Unit = {
    apply(a)
    apply(b)
  }

  def apply[A, B, C](a: A, b: B, c: C)(implicit
    ra: R[A],
    rb: R[B],
    rc: R[C],
    builder: Builder
  ): Unit = {
    apply(a)
    apply(b)
    apply(c)
  }

  def apply[A, B, C, D](a: A, b: B, c: C, d: D)(implicit
    ra: R[A],
    rb: R[B],
    rc: R[C],
    rd: R[D],
    builder: Builder
  ): Unit = {
    apply(a)
    apply(b)
    apply(c)
    apply(d)
  }
}
