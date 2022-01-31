package zio.sql

trait UtilsModule { self =>
  sealed trait TrailingUnitNormalizer[In] {
    type Out
  }

  object TrailingUnitNormalizer {
    type WithOut[In0, Out0] = TrailingUnitNormalizer[In0] {
      type Out = Out0
    }
    // format: off
    implicit def arity1[In, A](implicit ev: In =:= (A, Unit)) : TrailingUnitNormalizer.WithOut[In, A] = new TrailingUnitNormalizer[In] {
      override type Out = A
    }

    implicit def arity2[In, A, B](implicit ev: In =:= (A, (B, Unit))) : TrailingUnitNormalizer.WithOut[In, (A, B)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B)
    }

    implicit def arity3[In, A, B, C](implicit ev: In =:= (A, (B, (C, Unit)))) : TrailingUnitNormalizer.WithOut[In, (A, B, C)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C)
    }

    implicit def arity4[In, A, B, C, D](implicit ev: In =:= (A, (B, (C, (D, Unit))))) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D)
    }

    implicit def arity5[In, A, B, C, D, E](implicit ev: In =:= (A, (B, (C, (D, (E, Unit)))))) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E)
    }

    implicit def arity6[In, A, B, C, D, E, F](implicit ev: In =:= (A, (B, (C, (D, (E, (F, Unit))))))) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F)
    }

    implicit def arity7[In, A, B, C, D, E, F, G](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, Unit)))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G)
    }

    implicit def arity8[In, A, B, C, D, E, F, G, H](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H)
    }

    implicit def arity9[In, A, B, C, D, E, F, G, H, I](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I)
    }

    implicit def arity10[In, A, B, C, D, E, F, G, H, I, J](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J)
    }

    implicit def arity11[In, A, B, C, D, E, F, G, H, I, J, K](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit)))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K)
    }

    implicit def arity12[In, A, B, C, D, E, F, G, H, I, J, K, L](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L)
    }

    implicit def arity13[In, A, B, C, D, E, F, G, H, I, J, K, L, M](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M)
    }

    implicit def arity14[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
    }

    implicit def arity15[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
    }

    implicit def arity16[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
    }

    implicit def arity17[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
    }

    implicit def arity18[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit))))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
    }

    implicit def arity19[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit)))))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
    }

    implicit def arity20[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit))))))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
    }

    implicit def arity21[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit)))))))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
    }

    implicit def arity22[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit ev: In =:= (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit))))))))))))))))))))))): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
    }
    // format: on
  }
}
