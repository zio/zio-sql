package zio.sql.typetag

import java.time._

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
