package zio.sql.utils


  sealed trait TrailingUnitNormalizer[In] {
    type Out

    def apply(in: In): Out
  }

  object TrailingUnitNormalizer {
    type WithOut[In0, Out0] = TrailingUnitNormalizer[In0] {
      type Out = Out0
    }
    // format: off
    implicit def arity1[In, A]: TrailingUnitNormalizer.WithOut[(A, Unit), A] = new TrailingUnitNormalizer[(A, Unit)] {
      override type Out = A

      override def apply(in: (A, Unit)): A = 
        in._1
    }

    implicit def arity2[In, A, B]: TrailingUnitNormalizer.WithOut[(A, (B, Unit)), (A, B)] = new TrailingUnitNormalizer[(A, (B, Unit))] {
      override type Out = (A, B)

      override def apply(in: (A, (B, Unit))): Out = 
        (in._1, in._2._1)
    }

    implicit def arity3[In, A, B, C]: TrailingUnitNormalizer.WithOut[(A, (B, (C, Unit))), (A, B, C)] = new TrailingUnitNormalizer[(A, (B, (C, Unit)))] {
      override type Out = (A, B, C)

      override def apply(in: (A, (B, (C, Unit))) ): Out = 
        (in._1, in._2._1, in._2._2._1)
    }

    implicit def arity4[In, A, B, C, D]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, Unit)))), (A, B, C, D)] = new TrailingUnitNormalizer[(A, (B, (C, (D, Unit))))] {
      override type Out = (A, B, C, D)

      override def apply(in: (A, (B, (C, (D, Unit))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1)
    }

    implicit def arity5[In, A, B, C, D, E]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, Unit))))), (A, B, C, D, E)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, Unit)))))] {
      override type Out = (A, B, C, D, E)

      override def apply(in: (A, (B, (C, (D, (E, Unit)))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1)
    }

    implicit def arity6[In, A, B, C, D, E, F]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, Unit)))))), (A, B, C, D, E, F)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, Unit))))))] {
      override type Out = (A, B, C, D, E, F)

      override def apply(in: (A, (B, (C, (D, (E, (F, Unit))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1,  in._2._2._2._2._2._1)
    }

    implicit def arity7[In, A, B, C, D, E, F, G]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, Unit))))))), (A, B, C, D, E, F, G)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, Unit)))))))] {
      override type Out = (A, B, C, D, E, F, G)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, Unit)))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1)
    }

    implicit def arity8[In, A, B, C, D, E, F, G, H]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, Unit)))))))), (A, B, C, D, E, F, G, H)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))] {
      override type Out = (A, B, C, D, E, F, G, H)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))): Out =
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1)
    }

    implicit def arity9[In, A, B, C, D, E, F, G, H, I]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, Unit))))))))), (A, B, C, D, E, F, G, H, I)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity10[In, A, B, C, D, E, F, G, H, I, J]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit)))))))))), (A, B, C, D, E, F, G, H, I, J)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity11[In, A, B, C, D, E, F, G, H, I, J, K]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit))))))))))), (A, B, C, D, E, F, G, H, I, J, K)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit)))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit)))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity12[In, A, B, C, D, E, F, G, H, I, J, K, L]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit)))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity13[In, A, B, C, D, E, F, G, H, I, J, K, L, M]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M)

      override def apply(in:  (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity14[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit)))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity15[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity16[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit)))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit)))))))))))))))) ): Out =
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity17[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity18[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit)))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit))))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit))))))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity19[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit)))))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))) ): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity20[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit)))))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit))))))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit))))))))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity21[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit))))))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit)))))))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit)))))))))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }

    implicit def arity22[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]: TrailingUnitNormalizer.WithOut[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit)))))))))))))))))))))), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = new TrailingUnitNormalizer[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit))))))))))))))))))))))] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)

      override def apply(in: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit))))))))))))))))))))))): Out = 
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
    }
    // format: on
  }