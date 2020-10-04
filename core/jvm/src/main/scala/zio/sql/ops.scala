package zio.sql

trait OpsModule extends TypeTagModule {

  sealed trait UnaryOp[A] extends Renderable {
    val symbol: String
    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      val _ = builder.append(" ").append(symbol)
    }
  }

  object UnaryOp {
    sealed case class Negate[A: IsNumeric]() extends UnaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]
      val symbol                  = "-"
    }

    sealed case class NotBit[A: IsIntegral]() extends UnaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      val symbol                    = "~"
    }

    case object NotBool extends UnaryOp[Boolean] {
      val symbol = "not"
    }
  }

  sealed trait BinaryOp[A] extends Renderable {
    val symbol: String

    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      val _ = builder.append(" ").append(symbol).append(" ")
    }
  }

  object BinaryOp {

    sealed case class Add[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "+"
    }

    sealed case class Sub[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "-"
    }

    sealed case class Mul[A: IsNumeric]() extends BinaryOp[A] {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "*"
    }

    sealed case class Div[A: IsNumeric]() extends BinaryOp[A]       {
      def isNumeric: IsNumeric[A] = implicitly[IsNumeric[A]]

      override val symbol: String = "/"
    }
    case object AndBool                   extends BinaryOp[Boolean] {
      override val symbol: String = "and"
    }

    case object OrBool extends BinaryOp[Boolean] {
      override val symbol: String = "or"
    }

    sealed case class AndBit[A: IsIntegral]() extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      override val symbol: String   = "&"
    }
    sealed case class OrBit[A: IsIntegral]()  extends BinaryOp[A] {
      def isIntegral: IsIntegral[A] = implicitly[IsIntegral[A]]
      override val symbol: String   = "|"

    }
  }

  sealed trait PropertyOp extends Renderable {
    val symbol: String

    override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
      val _ = builder.append(" ").append(symbol)
    }
  }

  object PropertyOp {
    case object IsNull    extends PropertyOp {
      override val symbol: String = "is null"
    }
    case object IsNotNull extends PropertyOp {
      override val symbol: String = "is not null"
    }
    //todo how is this different to "= true"?
    case object IsTrue    extends PropertyOp {
      override val symbol: String = "= true"
    }
    case object IsNotTrue extends PropertyOp {
      override val symbol: String = "= false"
    }
  }

  sealed trait RelationalOp extends Renderable

  object RelationalOp {
    case object Equals           extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" = ")
      }
    }
    case object LessThan         extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" < ")
      }
    }
    case object GreaterThan      extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" > ")
      }
    }
    case object LessThanEqual    extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" <= ")
      }
    }
    case object GreaterThanEqual extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" >= ")
      }
    }
    case object NotEqual         extends RelationalOp {
      override private[zio] def renderBuilder(builder: StringBuilder, mode: RenderMode): Unit = {
        val _ = builder.append(" <> ")
      }
    }
  }

}
