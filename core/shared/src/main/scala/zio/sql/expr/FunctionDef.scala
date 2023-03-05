package zio.sql.expr

import java.time._
import zio.sql.typetag.TypeTag
import zio.sql.Features

final case class FunctionDef[-A, +B](name: FunctionName) { self =>

  def apply[B1 >: B]()(implicit ev: Any <:< A, typeTag: TypeTag[B1]): Expr[Features.Function0, Any, B1] =
    Expr.FunctionCall0(self.asInstanceOf[FunctionDef[Any, B1]])

  def apply[F, Source, B1 >: B](param1: Expr[F, Source, A])(implicit typeTag: TypeTag[B1]): Expr[F, Source, B1] =
    Expr.FunctionCall1(param1, self: FunctionDef[A, B1])

  def apply[F1, F2, Source, P1, P2, B1 >: B](param1: Expr[F1, Source, P1], param2: Expr[F2, Source, P2])(implicit
    ev: (P1, P2) <:< A,
    typeTag: TypeTag[B1]
  ): Expr[F1 with F2, Source, B1] =
    Expr.FunctionCall2(param1, param2, self.narrow[(P1, P2)]: FunctionDef[(P1, P2), B1])

  def apply[F1, F2, F3, Source, P1, P2, P3, B1 >: B](
    param1: Expr[F1, Source, P1],
    param2: Expr[F2, Source, P2],
    param3: Expr[F3, Source, P3]
  )(implicit ev: (P1, P2, P3) <:< A, typeTag: TypeTag[B1]): Expr[F1 with F2 with F3, Source, B1] =
    Expr.FunctionCall3(param1, param2, param3, self.narrow[(P1, P2, P3)]: FunctionDef[(P1, P2, P3), B1])

  def apply[F1, F2, F3, F4, Source, P1, P2, P3, P4, B1 >: B](
    param1: Expr[F1, Source, P1],
    param2: Expr[F2, Source, P2],
    param3: Expr[F3, Source, P3],
    param4: Expr[F4, Source, P4]
  )(implicit ev: (P1, P2, P3, P4) <:< A, typeTag: TypeTag[B1]): Expr[F1 with F2 with F3 with F4, Source, B1] =
    Expr.FunctionCall4(
      param1,
      param2,
      param3,
      param4,
      self.narrow[(P1, P2, P3, P4)]: FunctionDef[(P1, P2, P3, P4), B1]
    )

  def apply[F1, F2, F3, F4, F5, Source, P1, P2, P3, P4, P5, B1 >: B](
    param1: Expr[F1, Source, P1],
    param2: Expr[F2, Source, P2],
    param3: Expr[F3, Source, P3],
    param4: Expr[F4, Source, P4],
    param5: Expr[F5, Source, P5]
  )(implicit
    ev: (P1, P2, P3, P4, P5) <:< A,
    typeTag: TypeTag[B1]
  ): Expr[F1 with F2 with F3 with F4 with F5, Source, B1] =
    Expr.FunctionCall5(
      param1,
      param2,
      param3,
      param4,
      param5,
      self.narrow[(P1, P2, P3, P4, P5)]: FunctionDef[(P1, P2, P3, P4, P5), B1]
    )

  def apply[F1, F2, F3, F4, F5, F6, Source, P1, P2, P3, P4, P5, P6, B1 >: B](
    param1: Expr[F1, Source, P1],
    param2: Expr[F2, Source, P2],
    param3: Expr[F3, Source, P3],
    param4: Expr[F4, Source, P4],
    param5: Expr[F5, Source, P5],
    param6: Expr[F6, Source, P6]
  )(implicit
    ev: (P1, P2, P3, P4, P5, P6) <:< A,
    typeTag: TypeTag[B1]
  ): Expr[F1 with F2 with F3 with F4 with F5 with F6, Source, B1] =
    Expr.FunctionCall6(
      param1,
      param2,
      param3,
      param4,
      param5,
      param6,
      self.narrow[(P1, P2, P3, P4, P5, P6)]: FunctionDef[(P1, P2, P3, P4, P5, P6), B1]
    )

  def apply[F1, F2, F3, F4, F5, F6, F7, Source, P1, P2, P3, P4, P5, P6, P7, B1 >: B](
    param1: Expr[F1, Source, P1],
    param2: Expr[F2, Source, P2],
    param3: Expr[F3, Source, P3],
    param4: Expr[F4, Source, P4],
    param5: Expr[F5, Source, P5],
    param6: Expr[F6, Source, P6],
    param7: Expr[F7, Source, P7]
  )(implicit
    ev: (P1, P2, P3, P4, P5, P6, P7) <:< A,
    typeTag: TypeTag[B1]
  ): Expr[F1 with F2 with F3 with F4 with F5 with F6 with F7, Source, B1] =
    Expr.FunctionCall7(
      param1,
      param2,
      param3,
      param4,
      param5,
      param6,
      param7,
      self.narrow[(P1, P2, P3, P4, P5, P6, P7)]: FunctionDef[(P1, P2, P3, P4, P5, P6, P7), B1]
    )

  def narrow[C](implicit ev: C <:< A): FunctionDef[C, B] = {
    val _ = ev
    self.asInstanceOf[FunctionDef[C, B]]
  }
}

object FunctionDef {

  // math functions
  val Abs         = FunctionDef[Double, Double](FunctionName("abs"))
  val Acos        = FunctionDef[Double, Double](FunctionName("acos"))
  val Asin        = FunctionDef[Double, Double](FunctionName("asin"))
  val Atan        = FunctionDef[Double, Double](FunctionName("atan"))
  val Ceil        = FunctionDef[Double, Double](FunctionName("ceil"))
  val Cos         = FunctionDef[Double, Double](FunctionName("cos"))
  val Exp         = FunctionDef[Double, Double](FunctionName("exp"))
  val Floor       = FunctionDef[Double, Double](FunctionName("floor"))
  val Ln          = FunctionDef[Double, Double](FunctionName("ln"))
  val Log         = FunctionDef[(Double, Double), Double](FunctionName("log"))
  val Mod         = FunctionDef[(Double, Double), Double](FunctionName("mod"))
  val Power       = FunctionDef[(Double, Double), Double](FunctionName("power"))
  val Round       = FunctionDef[(Double, Int), Double](FunctionName("round"))
  val Sign        = FunctionDef[Double, Int](FunctionName("sign"))
  val Sin         = FunctionDef[Double, Double](FunctionName("sin"))
  val Sqrt        = FunctionDef[Double, Double](FunctionName("sqrt"))
  val Tan         = FunctionDef[Double, Double](FunctionName("tan"))
  val WidthBucket = FunctionDef[(Double, Double, Double, Int), Int](FunctionName("width_bucket"))

  // string functions
  val Ascii       = FunctionDef[String, Int](FunctionName("ascii"))
  val CharLength  = FunctionDef[String, Int](FunctionName("character_length"))
  val Concat      = FunctionDef[(String, String), String](FunctionName("concat")) // todo varargs
  val ConcatWs3   = FunctionDef[(String, String, String), String](FunctionName("concat_ws"))
  val ConcatWs4   = FunctionDef[(String, String, String, String), String](FunctionName("concat_ws"))
  val Lower       = FunctionDef[String, String](FunctionName("lower"))
  val Ltrim       = FunctionDef[String, String](FunctionName("ltrim"))
  val OctetLength = FunctionDef[String, Int](FunctionName("octet_length"))
  val Overlay     = FunctionDef[(String, String, Int, Option[Int]), String](FunctionName("overlay"))
  val Position    = FunctionDef[(String, String), Int](FunctionName("position"))
  val Replace     = FunctionDef[(String, String, String), String](FunctionName("replace"))
  val Rtrim       = FunctionDef[String, String](FunctionName("rtrim"))
  val Substring   = FunctionDef[(String, Int, Option[Int]), String](FunctionName("substring"))
  // TODO substring regex
  val Trim        = FunctionDef[String, String](FunctionName("trim"))
  val Upper       = FunctionDef[String, String](FunctionName("upper"))

  // date functions
  val CurrentTimestamp = FunctionDef[Nothing, Instant](FunctionName("current_timestamp"))
}
