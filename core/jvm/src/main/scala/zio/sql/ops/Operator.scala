package zio.sql.ops

import zio.sql.typetag._

trait Operator {
  val symbol: String
}

object Operator {

  sealed trait BinaryOp[A] extends Operator {
    val symbol: String
  }

  object BinaryOp {

    final case class Add[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "+"
    }

    final case class Sub[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "-"
    }

    final case class Mul[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "*"
    }

    final case class Div[A: IsNumeric]() extends BinaryOp[A]       {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "/"
    }
    case object AndBool                  extends BinaryOp[Boolean] {
      override val symbol: String = "and"
    }

    case object OrBool extends BinaryOp[Boolean] {
      override val symbol: String = "or"
    }

    final case class AndBit[A: IsIntegral]() extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      override val symbol: String   = "&"
    }
    final case class OrBit[A: IsIntegral]()  extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      override val symbol: String   = "|"

    }
  }

  sealed trait PropertyOp extends Operator

  object PropertyOp {
    case object IsNull    extends PropertyOp {
      override val symbol: String = "is null"
    }
    case object IsNotNull extends PropertyOp {
      override val symbol: String = "is not null"
    }
    case object IsTrue    extends PropertyOp {
      override val symbol: String = "= true"
    }
    case object IsNotTrue extends PropertyOp {
      override val symbol: String = "= false"
    }
  }

  sealed trait RelationalOp extends Operator

  object RelationalOp {
    case object Equals           extends RelationalOp {
      override val symbol: String = "="
    }
    case object LessThan         extends RelationalOp {
      override val symbol: String = "<"
    }
    case object GreaterThan      extends RelationalOp {
      override val symbol: String = ">"
    }
    case object LessThanEqual    extends RelationalOp {
      override val symbol: String = "<="
    }
    case object GreaterThanEqual extends RelationalOp {
      override val symbol: String = ">="
    }
    case object NotEqual         extends RelationalOp {
      override val symbol: String = "<>"
    }
    case object Like             extends RelationalOp {
      override val symbol: String = "like"
    }
  }

  sealed trait UnaryOp[A] extends Operator

  object UnaryOp {
    final case class Negate[A: IsNumeric]() extends UnaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
      val symbol                  = "-"
    }

    final case class NotBit[A: IsIntegral]() extends UnaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      val symbol                    = "~"
    }

    case object NotBool extends UnaryOp[Boolean] {
      val symbol = "not"
    }
  }

}
