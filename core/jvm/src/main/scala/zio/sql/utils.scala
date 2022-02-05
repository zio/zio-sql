package zio.sql

trait UtilsModule { self =>
  sealed trait TrailingUnitNormalizer[In] {
    type Out

    def apply(in: In): Out
  }

  object TrailingUnitNormalizer {
    type WithOut[In0, Out0] = TrailingUnitNormalizer[In0] {
      type Out = Out0
    }
    // format: off
    //ev needs to be reversed because scala 2.12 cannot prove In =:= (A, Unit), therefore asInstanceOf
    implicit def arity1[In, A](implicit ev: (A, Unit) =:= In) : TrailingUnitNormalizer.WithOut[In, A] = new TrailingUnitNormalizer[In] {
      override type Out = A

      override def apply(in0: In): A = {
        val in = in0.asInstanceOf[(A, Unit)]

        in._1
      }
    }

    implicit def arity2[In, A, B](implicit ev: (A, (B, Unit)) =:= In) : TrailingUnitNormalizer.WithOut[In, (A, B)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, Unit))]

        (in._1, in._2._1)
      }
    }

    implicit def arity3[In, A, B, C](implicit ev: (A, (B, (C, Unit))) =:= In) : TrailingUnitNormalizer.WithOut[In, (A, B, C)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, Unit)))]

        (in._1, in._2._1, in._2._2._1)
      }
    }

    implicit def arity4[In, A, B, C, D](implicit ev: (A, (B, (C, (D, Unit)))) =:= In) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, Unit))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1)
      }
    }

    implicit def arity5[In, A, B, C, D, E](implicit ev: (A, (B, (C, (D, (E, Unit))))) =:= In) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, Unit)))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1)
      }
    }

    implicit def arity6[In, A, B, C, D, E, F](implicit ev: (A, (B, (C, (D, (E, (F, Unit)))))) =:= In) : TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, Unit))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1,  in._2._2._2._2._2._1)
      }
    }

    implicit def arity7[In, A, B, C, D, E, F, G](implicit ev: (A, (B, (C, (D, (E, (F, (G, Unit))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, Unit)))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1)
      }
    }

    implicit def arity8[In, A, B, C, D, E, F, G, H](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, Unit)))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity9[In, A, B, C, D, E, F, G, H, I](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))]
        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity10[In, A, B, C, D, E, F, G, H, I, J](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit)))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, Unit))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity11[In, A, B, C, D, E, F, G, H, I, J, K](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, Unit)))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity12[In, A, B, C, D, E, F, G, H, I, J, K, L](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit)))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity13[In, A, B, C, D, E, F, G, H, I, J, K, L, M](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity14[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit)))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity15[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity16[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit)))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity17[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity18[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit)))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit))))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity19[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit)))))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity20[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit)))))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit))))))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity21[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit))))))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, Unit)))))))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }

    implicit def arity22[In, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit ev: (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit)))))))))))))))))))))) =:= In): TrailingUnitNormalizer.WithOut[In, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = new TrailingUnitNormalizer[In] {
      override type Out = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)

      override def apply(in0: In): Out = {
        val in = in0.asInstanceOf[(A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, Unit))))))))))))))))))))))]

        (in._1, in._2._1, in._2._2._1, in._2._2._2._1, in._2._2._2._2._1, in._2._2._2._2._2._1, in._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1, in._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._2._1)
      }
    }
    // format: on
  }
}
