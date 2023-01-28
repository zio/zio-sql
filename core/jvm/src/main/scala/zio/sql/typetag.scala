package zio.sql

import java.sql.ResultSet
import java.time._
import java.util.UUID
import zio.Chunk

trait TypeTagModule { self: SelectModule =>

  type TypeTagExtension[+A] <: Tag[A] with Decodable[A]

  trait Decodable[+A] {
    def decode(column: Int, resultSet: ResultSet): Either[DecodingError, A]
  }

  trait Tag[+A] {
    private[zio] def cast(a: Any): A = a.asInstanceOf[A]
  }
  sealed trait TypeTag[+A] extends Tag[A]

  object TypeTag {
    sealed trait NotNull[+A]             extends TypeTag[A]
    implicit case object TBigDecimal     extends NotNull[java.math.BigDecimal]
    implicit case object TBoolean        extends NotNull[Boolean]
    implicit case object TByte           extends NotNull[Byte]
    implicit case object TByteArray      extends NotNull[Chunk[Byte]]
    implicit case object TChar           extends NotNull[Char]
    implicit case object TDouble         extends NotNull[Double]
    implicit case object TFloat          extends NotNull[Float]
    implicit case object TInstant        extends NotNull[Instant]
    implicit case object TInt            extends NotNull[Int]
    implicit case object TLocalDate      extends NotNull[LocalDate]
    implicit case object TLocalDateTime  extends NotNull[LocalDateTime]
    implicit case object TLocalTime      extends NotNull[LocalTime]
    implicit case object TLong           extends NotNull[Long]
    implicit case object TOffsetDateTime extends NotNull[OffsetDateTime]
    implicit case object TOffsetTime     extends NotNull[OffsetTime]
    implicit case object TShort          extends NotNull[Short]
    implicit case object TString         extends NotNull[String]
    implicit case object TUUID           extends NotNull[UUID]
    implicit case object TZonedDateTime  extends NotNull[ZonedDateTime]

    // TODO how to handle dialect specific in tablelike macro ?
    sealed case class TDialectSpecific[+A](typeTagExtension: TypeTagExtension[A]) extends NotNull[A]
    sealed case class Nullable[A: NotNull]()                                      extends TypeTag[Option[A]] {
      def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
    }
    implicit case object TNone                                                    extends TypeTag[None.type]

    implicit def option[A: NotNull]: TypeTag[Option[A]] = Nullable[A]()

    implicit def dialectSpecific[A](implicit typeTagExtension: TypeTagExtension[A]): TypeTag[A] =
      TDialectSpecific(typeTagExtension)
  }

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

  sealed trait IsDate[A] {
    def typeTag: TypeTag[A]
  }

  object IsDate {
    abstract class AbstractIsDate[A: TypeTag] extends IsDate[A] {
      def typeTag = implicitly[TypeTag[A]]
    }
    implicit case object InstantIsDate        extends AbstractIsDate[Instant]
    implicit case object LocalDateIsDate      extends AbstractIsDate[LocalDate]
    implicit case object LocalDateTimeIsDate  extends AbstractIsDate[LocalDateTime]
    implicit case object LocalTimeIsDate      extends AbstractIsDate[LocalTime]
    implicit case object OffsetDateTimeIsDate extends AbstractIsDate[OffsetDateTime]
    implicit case object OffsetTimeIsDate     extends AbstractIsDate[OffsetTime]
    implicit case object ZonedDateTimeIsDate  extends AbstractIsDate[ZonedDateTime]
  }
}
