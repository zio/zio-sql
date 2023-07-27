package zio.sql.typetag

import java.time._
import java.util.UUID
import zio.Chunk
import zio.schema._

trait Tag[+A] {
  private[zio] def cast(a: Any): A = a.asInstanceOf[A]
}

sealed trait TypeTag[+A] extends Tag[A]

object TypeTag {

  trait TypeTagExtension[+A] extends Tag[A] with Decodable[A]

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
  final case class TDialectSpecific[+A](typeTagExtension: TypeTagExtension[A]) extends NotNull[A]
  final case class Nullable[A: NotNull]()                                      extends TypeTag[Option[A]] {
    def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
  }
  implicit case object TNone                                                   extends TypeTag[None.type]

  implicit def option[A: NotNull]: TypeTag[Option[A]] = Nullable[A]()

  implicit def dialectSpecific[A](implicit typeTagExtension: TypeTagExtension[A]): TypeTag[A] =
    TDialectSpecific(typeTagExtension)

  def deriveTypeTag[A](standardType: StandardType[A]): Option[TypeTag.NotNull[A]] =
    standardType match {
      case StandardType.BigDecimalType     => Some(TypeTag.TBigDecimal)
      case StandardType.BoolType           => Some(TypeTag.TBoolean)
      case StandardType.ByteType           => Some(TypeTag.TByte)
      case StandardType.BinaryType         => Some(TypeTag.TByteArray)
      case StandardType.CharType           => Some(TypeTag.TChar)
      case StandardType.DoubleType         => Some(TypeTag.TDouble)
      case StandardType.FloatType          => Some(TypeTag.TFloat)
      case StandardType.InstantType        => Some(TypeTag.TInstant)
      case StandardType.IntType            => Some(TypeTag.TInt)
      case StandardType.LocalDateType      => Some(TypeTag.TLocalDate)
      case StandardType.LocalDateTimeType  => Some(TypeTag.TLocalDateTime)
      case StandardType.OffsetTimeType     => Some(TypeTag.TOffsetTime)
      case StandardType.LocalTimeType      => Some(TypeTag.TLocalTime)
      case StandardType.LongType           => Some(TypeTag.TLong)
      case StandardType.OffsetDateTimeType => Some(TypeTag.TOffsetDateTime)
      case StandardType.ShortType          => Some(TypeTag.TShort)
      case StandardType.StringType         => Some(TypeTag.TString)
      case StandardType.UUIDType           => Some(TypeTag.TUUID)
      case StandardType.ZonedDateTimeType  => Some(TypeTag.TZonedDateTime)
      // TODO What other types to support ?
      case StandardType.BigIntegerType     => None
      case StandardType.ZoneOffsetType     => None
      case StandardType.DurationType       => None
      case StandardType.YearType           => None
      case StandardType.MonthType          => None
      case StandardType.MonthDayType       => None
      case StandardType.ZoneIdType         => None
      case StandardType.PeriodType         => None
      case StandardType.YearMonthType      => None
      case StandardType.DayOfWeekType      => None
      case StandardType.UnitType           => None
    }

  def deriveTypeTag[A](opSchema: Schema.Optional[A]): Option[TypeTag[Option[A]]] =
    opSchema.schema match {
      case Schema.Primitive(standardType, _) =>
        implicit val notNullTypeTag = deriveTypeTag(standardType).get

        Some(TypeTag.option[A])
      case _                                 => None
    }

  def deriveTypeTag[A](fieldSchema: Schema[A]): Option[TypeTag[A]] =
    fieldSchema match {
      case s: Schema.Optional[_]                      => deriveTypeTag(s)
      case s: Schema.Lazy[A]                          => deriveTypeTag(s.schema)
      case Schema.Primitive(standardType, _)          => deriveTypeTag(standardType)
      case Schema.Sequence(elementSchema, _, _, _, _) =>
        elementSchema match {
          case Schema.Primitive(standardType, _) if (standardType == StandardType.ByteType) =>
            Some(TypeTag.TByteArray.asInstanceOf[TypeTag[A]])
          case _                                                                            => None
        }

      // TODO get TypeTag of A available out of Schema[A] and derive typetag from Schema.Transform
      case _: Schema.Transform[_, _, _]               => None
      case _                                          => None
    }
}
