package zio.sql.typetag

sealed trait IsIntegral[A] {
  def typeTag: TypeTag[A]
}

object IsIntegral {

  abstract class AbstractIsIntegral[A: TypeTag] extends IsIntegral[A] {
    def typeTag = implicitly[TypeTag[A]]
  }
  implicit case object TByteIsIntegral          extends AbstractIsIntegral[Byte]
  implicit case object TShortIsIntegral         extends AbstractIsIntegral[Short]
  implicit case object TIntIsIntegral           extends AbstractIsIntegral[Int]
  implicit case object TLongIsIntegral          extends AbstractIsIntegral[Long]
}
