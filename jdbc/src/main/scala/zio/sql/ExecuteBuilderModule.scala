package zio.sql

import zio._

trait ExecuteBuilderModule { self: Jdbc =>

  class ExecuteBuilder[Set <: SelectionSet[_], Output](val read: Read.Aux[Output, Set]) {
    import zio.stream._

    def to[A, Target](f: A => Target)(implicit ev: Output <:< (A, Unit)): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, _) = ev(resultType)

        f(a)
      }))

    def to[A, B, Target](
      f: (A, B) => Target
    )(implicit ev: Output <:< (A, (B, Unit))): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, _)) = ev(resultType)

        f(a, b)
      }))

    def to[A, B, C, Target](
      f: (A, B, C) => Target
    )(implicit ev: Output <:< (A, (B, (C, Unit)))): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, _))) = ev(resultType)

        f(a, b, c)
      }))

    def to[A, B, C, D, Target](
      f: (A, B, C, D) => Target
    )(implicit ev: Output <:< (A, (B, (C, (D, Unit))))): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, _)))) = ev(resultType)

        f(a, b, c, d)
      }))

    def to[A, B, C, D, E, Target](
      f: (A, B, C, D, E) => Target
    )(implicit ev: Output <:< (A, (B, (C, (D, (E, Unit)))))): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, _))))) = ev(resultType)

        f(a, b, c, d, e)
      }))

    def to[A, B, C, D, E, F, G, H, Target](
      f: (A, B, C, D, E, F, G, H) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, _)))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h)
      }))

    def to[A, B, C, D, E, F, G, H, I, Target](
      f: (A, B, C, D, E, F, G, H, I) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, _))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, _)))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, _))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, _)))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, _))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, _)))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Target
    )(implicit
      ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, _))))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Target
    )(implicit
      ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit)))))))))))))))))
      )
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, _)))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Target
    )(implicit
      ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))
      )
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, _))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Target
    )(implicit
      ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit)))))))))))))))))))
      )
    ): ZStream[Has[SqlExecutor], Exception, Target] =
      ZStream.unwrap(ZIO.access[Has[SqlExecutor]](_.get.read(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, (t, _)))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      }))
  }
}
