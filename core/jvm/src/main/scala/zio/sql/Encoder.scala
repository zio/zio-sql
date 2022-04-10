package zio.sql

trait Encoder[A] {
  def render(value: A): String = String.valueOf(value)
}

object Encoder {
  implicit case object EBigDecimal extends Encoder[BigDecimal]
  implicit case object EBoolean    extends Encoder[Boolean]
  implicit case object EDouble     extends Encoder[Double]
  implicit case object EFloat      extends Encoder[Float]
  implicit case object EInt        extends Encoder[Int]
  implicit case object ELong       extends Encoder[Long]
  implicit case object EShort      extends Encoder[Short]

  implicit def nullable[A: Encoder]: Encoder[Option[A]] = new Encoder[Option[A]] {
    override def render(value: Option[A]): String = value.fold("NULL")(implicitly[Encoder[A]].render)
  }
}
