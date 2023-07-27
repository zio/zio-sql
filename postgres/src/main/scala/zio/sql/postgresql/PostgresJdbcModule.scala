package zio.sql.postgresql

import java.sql.Timestamp
import java.time._
import java.util.UUID

import zio.schema.StandardType._
import zio.sql.{ Jdbc, SqlParameter }

trait PostgresJdbcModule extends PostgresRenderModule with Jdbc {

  override def setParam(param: SqlParameter, jdbcIndex: Int): java.sql.PreparedStatement => Unit = ps =>
    param._type match {
      case BigDecimalType     => ps.setBigDecimal(jdbcIndex, param.value.asInstanceOf[java.math.BigDecimal])
      case InstantType        => ps.setTimestamp(jdbcIndex, Timestamp.from(param.value.asInstanceOf[Instant]))
      case ByteType           => ps.setByte(jdbcIndex, param.value.asInstanceOf[Byte])
      case CharType           => ps.setString(jdbcIndex, String.valueOf(param.value.asInstanceOf[Char]))
      case IntType            => ps.setInt(jdbcIndex, param.value.asInstanceOf[Int])
      case MonthDayType       => ps.setString(jdbcIndex, param.value.toString())
      case BinaryType         => ps.setString(jdbcIndex, param.value.toString())
      case MonthType          => ps.setString(jdbcIndex, param.value.toString())
      case LocalDateTimeType  => ps.setObject(jdbcIndex, param.value.asInstanceOf[LocalDateTime])
      case UnitType           => ps.setObject(jdbcIndex, null)
      case YearMonthType      => ps.setString(jdbcIndex, param.value.toString())
      case DoubleType         => ps.setDouble(jdbcIndex, param.value.asInstanceOf[Double])
      case YearType           => ps.setString(jdbcIndex, param.value.toString())
      case OffsetDateTimeType => ps.setObject(jdbcIndex, param.value.asInstanceOf[OffsetDateTime])
      case ZonedDateTimeType  =>
        ps.setObject(jdbcIndex, Timestamp.from(param.value.asInstanceOf[ZonedDateTime].toInstant))
      case BigIntegerType     => ps.setLong(jdbcIndex, param.value.asInstanceOf[BigInt].longValue)
      case UUIDType           => ps.setObject(jdbcIndex, param.value.asInstanceOf[UUID])
      case ZoneOffsetType     => ps.setString(jdbcIndex, param.value.toString())
      case ShortType          => ps.setShort(jdbcIndex, param.value.asInstanceOf[Short])
      case LocalTimeType      => ps.setObject(jdbcIndex, param.value.asInstanceOf[LocalTime])
      case OffsetTimeType     => ps.setObject(jdbcIndex, param.value.asInstanceOf[OffsetTime])
      case LongType           => ps.setLong(jdbcIndex, param.value.asInstanceOf[Long])
      case StringType         => ps.setString(jdbcIndex, param.value.asInstanceOf[String])
      case PeriodType         => ps.setString(jdbcIndex, param.value.asInstanceOf[String])
      case ZoneIdType         => ps.setString(jdbcIndex, param.value.asInstanceOf[String])
      case LocalDateType      => ps.setObject(jdbcIndex, param.value.asInstanceOf[LocalDate])
      case BoolType           => ps.setBoolean(jdbcIndex, param.value.asInstanceOf[Boolean])
      case DayOfWeekType      => ps.setString(jdbcIndex, param.value.asInstanceOf[String])
      case FloatType          => ps.setFloat(jdbcIndex, param.value.asInstanceOf[Float])
      case DurationType       => ps.setString(jdbcIndex, param.value.asInstanceOf[String])
    }
}
