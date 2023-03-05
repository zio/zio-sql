package zio.sql.typetag

sealed trait IsNumeric[A] {
  def typeTag: TypeTag[A]
}

object IsNumeric {

  abstract class AbstractIsNumeric[A: TypeTag] extends IsNumeric[A] {
    def typeTag = implicitly[TypeTag[A]]
  }
  implicit case object TShortIsNumeric         extends AbstractIsNumeric[Short]
  implicit case object TIntIsNumeric           extends AbstractIsNumeric[Int]
  implicit case object TLongIsNumeric          extends AbstractIsNumeric[Long]
  implicit case object TFloatIsNumeric         extends AbstractIsNumeric[Float]
  implicit case object TDoubleIsNumeric        extends AbstractIsNumeric[Double]
  // TODO IS BigDecimal numeric? can I work in sql with -, + on `money` type?
  implicit case object TBigDecimalIsNumeric    extends AbstractIsNumeric[java.math.BigDecimal]
}
