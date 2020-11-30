package zio.sql.rendering

import zio.sql.Sql

trait RenderModule { self: Sql =>

  //not using vararg to avoid allocating `Seq`s
  def render[A](a: A)(implicit r: SqlRendering[A], builder: Builder): Unit = r.apply(a)(builder)

  def render[A, B](a: A, b: B)(implicit ra: SqlRendering[A], rb: SqlRendering[B], builder: Builder): Unit = {
    render(a)
    render(b)
  }

  def render[A, B, C](a: A, b: B, c: C)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
  }

  def render[A, B, C, D](a: A, b: B, c: C, d: D)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
  }
  def render[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
  }
  def render[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    rf: SqlRendering[F],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
    render(f)
  }
  def render[A, B, C, D, E, F, G](a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    rf: SqlRendering[F],
    rg: SqlRendering[G],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
    render(f)
    render(g)
  }
  def render[A, B, C, D, E, F, G, H, I](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    rf: SqlRendering[F],
    rg: SqlRendering[G],
    rh: SqlRendering[H],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
    render(f)
    render(g)
    render(h)
  }
  def render[A, B, C, D, E, F, G, H, I](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    rf: SqlRendering[F],
    rg: SqlRendering[G],
    rh: SqlRendering[H],
    ri: SqlRendering[I],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
    render(f)
    render(g)
    render(h)
    render(i)
  }
  def render[A, B, C, D, E, F, G, H, I, J](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit
    ra: SqlRendering[A],
    rb: SqlRendering[B],
    rc: SqlRendering[C],
    rd: SqlRendering[D],
    re: SqlRendering[E],
    rf: SqlRendering[F],
    rg: SqlRendering[G],
    rh: SqlRendering[H],
    ri: SqlRendering[I],
    rj: SqlRendering[J],
    builder: Builder
  ): Unit = {
    render(a)
    render(b)
    render(c)
    render(d)
    render(e)
    render(f)
    render(g)
    render(h)
    render(i)
    render(j)
  }
}
