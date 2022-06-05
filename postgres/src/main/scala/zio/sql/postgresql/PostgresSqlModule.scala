package zio.sql.postgresql

import java.sql.ResultSet
import java.text.DecimalFormat
import java.time._
import java.time.format.DateTimeFormatter
import java.util.{ Calendar, UUID }
import org.postgresql.util.PGInterval
import zio.Chunk
import zio.sql.Sql
import zio.schema._

trait PostgresSqlModule extends Sql { self =>
  import TypeTag._

  override type TypeTagExtension[+A] = PostgresSpecific.PostgresTypeTag[A]

  override type TableExtension[A] = PostgresSpecific.PostgresSpecificTable[A]

  object PostgresSpecific {
    sealed trait PostgresSpecificTable[A] extends Table.TableEx[A]

    object PostgresSpecificTable {
      import scala.language.implicitConversions

      sealed case class LateraLTable[A, B](left: Table.Aux[A], right: Table.Aux[B])
          extends PostgresSpecificTable[A with B] { self =>

        override type ColumnHead = left.ColumnHead

        override type HeadIdentity0 = left.HeadIdentity0
        override type ColumnTail    =
          left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail, right.HeadIdentity0]]

        override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] =
          left.columnSet ++ right.columnSet

        override val columnToExpr: ColumnToExpr[A with B] = new ColumnToExpr[A with B] {
          def toExpr[C](column: Column[C]): Expr[Features.Source[column.Identity, A with B], A with B, C] =
            if (left.columnSet.contains(column))
              left.columnToExpr
                .toExpr(column)
                .asInstanceOf[Expr[Features.Source[column.Identity, A with B], A with B, C]]
            else
              right.columnToExpr
                .toExpr(column)
                .asInstanceOf[Expr[Features.Source[column.Identity, A with B], A with B, C]]
        }
      }

      implicit def tableSourceToSelectedBuilder[A](
        table: Table.Aux[A]
      ): LateralTableBuilder[A] =
        new LateralTableBuilder(table)

      sealed case class LateralTableBuilder[A](left: Table.Aux[A]) {
        self =>

        final def lateral[Out](
          right: Table.DerivedTable[Out, Read[Out]]
        ): Table.DialectSpecificTable[A with right.TableType] = {

          val tableExtension = LateraLTable[A, right.TableType](
            left,
            right
          )

          new Table.DialectSpecificTable(tableExtension)
        }
      }
    }

    trait PostgresTypeTag[+A] extends Tag[A] with Decodable[A]
    object PostgresTypeTag {
      implicit case object TVoid       extends PostgresTypeTag[Unit]       {
        override def decode(column: Int, resultSet: ResultSet): Either[DecodingError, Unit] =
          scala.util
            .Try(resultSet.getObject(column))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              _ => Right(())
            )
      }
      implicit case object TInterval   extends PostgresTypeTag[Interval]   {
        override def decode(
          column: Int,
          resultSet: ResultSet
        ): Either[DecodingError, Interval] =
          scala.util
            .Try(Interval.fromPgInterval(new PGInterval(resultSet.getString(column))))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              r => Right(r)
            )
      }
      implicit case object TTimestampz extends PostgresTypeTag[Timestampz] {
        override def decode(
          column: Int,
          resultSet: ResultSet
        ): Either[DecodingError, Timestampz] =
          scala.util
            .Try(
              Timestampz.fromZonedDateTime(
                ZonedDateTime
                  .ofInstant(
                    resultSet.getTimestamp(column).toInstant,
                    ZoneId.of(ZoneOffset.UTC.getId)
                  )
              )
            )
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              r => Right(r)
            )
      }
    }

    sealed case class Timestampz(
      year: Int = 0,
      month: Int = 0,
      day: Int = 0,
      hour: Int = 0,
      minute: Int = 0,
      second: BigDecimal = 0.0,
      timeZone: String = "+00"
    ) {
      override def toString =
        s"""$year, $month, $day, $hour, $minute, $second, '$timeZone'"""
    }

    // Based upon https://github.com/tminglei/slick-pg/blob/master/src/main/scala/com/github/tminglei/slickpg/PgDateSupport.scala
    sealed case class Interval(
      years: Int = 0,
      months: Int = 0,
      days: Int = 0,
      hours: Int = 0,
      minutes: Int = 0,
      seconds: BigDecimal = 0.0
    ) { self =>
      private val secondsFormat = {
        val format = new DecimalFormat("0.00####")
        val dfs    = format.getDecimalFormatSymbols()
        dfs.setDecimalSeparator('.')
        format.setDecimalFormatSymbols(dfs)
        format
      }
      def milliseconds: Int     = (microseconds + (if (microseconds < 0) -500 else 500)) / 1000
      def microseconds: Int     = (seconds * 1000000.0).asInstanceOf[Int]

      def +:(cal: Calendar): Calendar = {
        cal.add(Calendar.MILLISECOND, milliseconds)
        cal.add(Calendar.MINUTE, minutes)
        cal.add(Calendar.HOUR, hours)
        cal.add(Calendar.DAY_OF_MONTH, days)
        cal.add(Calendar.MONTH, months)
        cal.add(Calendar.YEAR, years)
        cal
      }

      def +:(date: java.util.Date): java.util.Date = {
        val cal = Calendar.getInstance
        cal.setTime(date)
        date.setTime((cal +: self).getTime.getTime)
        date
      }

      def +(other: Interval): Interval =
        new Interval(
          years + other.years,
          months + other.months,
          days + other.days,
          hours + other.hours,
          minutes + other.minutes,
          seconds + other.seconds
        )

      def *(factor: Int): Interval =
        new Interval(
          years * factor,
          months * factor,
          days * factor,
          hours * factor,
          minutes * factor,
          seconds * factor
        )

      override def toString = {
        val secs = secondsFormat.format(seconds)
        s"""years => ${years}, months => ${months}, weeks => 0, days => ${days}, hours => ${hours}, mins => ${minutes}, secs => ${secs}"""
      }

      def fromPgInterval(interval: PGInterval): Interval =
        Interval(
          interval.getYears,
          interval.getMonths,
          interval.getDays,
          interval.getHours,
          interval.getMinutes,
          interval.getSeconds
        )
    }

    object Interval {
      def apply(interval: String): Interval              = fromPgInterval(new PGInterval(interval))
      def fromPgInterval(interval: PGInterval): Interval =
        new Interval(
          interval.getYears,
          interval.getMonths,
          interval.getDays,
          interval.getHours,
          interval.getMinutes,
          interval.getSeconds
        )

      def toPgInterval(interval: Interval): PGInterval =
        new PGInterval(
          interval.years,
          interval.months,
          interval.days,
          interval.hours,
          interval.minutes,
          interval.seconds.toDouble
        )
    }

    object Timestampz {
      def fromZonedDateTime(zdt: ZonedDateTime): Timestampz =
        Timestampz(
          zdt.getYear,
          zdt.getMonthValue,
          zdt.getDayOfMonth,
          zdt.getHour,
          zdt.getMinute,
          zdt.getSecond,
          zdt.getZone.getId
        )
    }
  }

  implicit val localDateSchema =
    Schema.primitive[LocalDate](zio.schema.StandardType.LocalDateType(DateTimeFormatter.ISO_DATE))

  implicit val zonedDateTimeShema =
    Schema.primitive[ZonedDateTime](zio.schema.StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  object PostgresFunctionDef {
    import PostgresSpecific._

    val BitLength                   = FunctionDef[String, Int](FunctionName("bit_length"))
    val CBRT                        = FunctionDef[Double, Double](FunctionName("cbrt"))
    val CharLength                  = FunctionDef[String, Int](FunctionName("character_length"))
    val Chr                         = FunctionDef[Int, String](FunctionName("chr"))
    val CurrentDate                 = Expr.ParenlessFunctionCall0[LocalDate](FunctionName("current_date"))
    val CurrentTime                 = Expr.ParenlessFunctionCall0[OffsetTime](FunctionName("current_time"))
    val Decode                      = FunctionDef[(String, String), Chunk[Byte]](FunctionName("decode"))
    val Degrees                     = FunctionDef[Double, Double](FunctionName("degrees"))
    val Div                         = FunctionDef[(Double, Double), Double](FunctionName("div"))
    val Encode                      = FunctionDef[(Chunk[Byte], String), String](FunctionName("encode"))
    val Factorial                   = FunctionDef[Int, Int](FunctionName("factorial"))
    val Format0                     = FunctionDef[String, String](FunctionName("format")) // TODO: varargs
    val Format1                     = FunctionDef[(String, Any), String](FunctionName("format"))
    val Format2                     = FunctionDef[(String, Any, Any), String](FunctionName("format"))
    val Format3                     = FunctionDef[(String, Any, Any, Any), String](FunctionName("format"))
    val Format4                     = FunctionDef[(String, Any, Any, Any, Any), String](FunctionName("format"))
    val Format5                     = FunctionDef[(String, Any, Any, Any, Any, Any), String](FunctionName("format"))
    val GCD                         = FunctionDef[(Double, Double), Double](FunctionName("gcd"))
    val GenRandomUuid               = Expr.FunctionCall0[UUID](FunctionDef[Any, UUID](FunctionName("gen_random_uuid")))
    val Hex                         = FunctionDef[Int, String](FunctionName("to_hex"))
    val Initcap                     = FunctionDef[String, String](FunctionName("initcap"))
    val IsFinite                    = FunctionDef[Instant, Boolean](FunctionName("isfinite"))
    val LCM                         = FunctionDef[(Double, Double), Double](FunctionName("lcm"))
    val Left                        = FunctionDef[(String, Int), String](FunctionName("left"))
    val Length                      = FunctionDef[String, Int](FunctionName("length"))
    val Localtime                   = Expr.ParenlessFunctionCall0[LocalTime](FunctionName("localtime"))
    val Localtimestamp              = Expr.ParenlessFunctionCall0[Instant](FunctionName("localtimestamp"))
    val LocaltimestampWithPrecision = FunctionDef[Int, Instant](FunctionName("localtimestamp"))
    val LocaltimeWithPrecision      = FunctionDef[Int, LocalTime](FunctionName("localtime"))
    val LPad                        = FunctionDef[(String, Int, String), String](FunctionName("lpad"))
    val MakeDate                    = FunctionDef[(Int, Int, Int), LocalDate](FunctionName("make_date"))
    val MakeInterval                = FunctionDef[Interval, Interval](FunctionName("make_interval"))
    val MakeTime                    = FunctionDef[(Int, Int, Double), LocalTime](FunctionName("make_time"))
    val MakeTimestamp               =
      FunctionDef[(Int, Int, Int, Int, Int, Double), LocalDateTime](
        FunctionName("make_timestamp")
      )
    val MakeTimestampz              =
      FunctionDef[Timestampz, Timestampz](FunctionName("make_timestamptz"))
    val Md5                         = FunctionDef[String, String](FunctionName("md5"))
    val MinScale                    = FunctionDef[Double, Int](FunctionName("min_scale"))
    val Now                         = FunctionDef[Any, ZonedDateTime](FunctionName("now"))
    val ParseIdent                  = FunctionDef[String, String](FunctionName("parse_ident"))
    val PgClientEncoding            = FunctionDef[Any, String](FunctionName("pg_client_encoding"))
    val Pi                          = Expr.FunctionCall0[Double](FunctionDef[Any, Double](FunctionName("pi")))
    val Radians                     = FunctionDef[Double, Double](FunctionName("radians"))
    val Random                      = FunctionDef[Any, Double](FunctionName("random"))
    val Repeat                      = FunctionDef[(String, Int), String](FunctionName("repeat"))
    val Reverse                     = FunctionDef[String, String](FunctionName("reverse"))
    val Right                       = FunctionDef[(String, Int), String](FunctionName("right"))
    val RPad                        = FunctionDef[(String, Int, String), String](FunctionName("rpad"))
    val SetSeed                     = FunctionDef[Double, Unit](FunctionName("setseed"))
    val Sind                        = FunctionDef[Double, Double](FunctionName("sind"))
    val SplitPart                   = FunctionDef[(String, String, Int), String](FunctionName("split_part"))
    val StartsWith                  = FunctionDef[(String, String), Boolean](FunctionName("starts_with"))
    val StatementTimestamp          = FunctionDef[Any, ZonedDateTime](FunctionName("statement_timestamp"))
    val TimeOfDay                   = FunctionDef[Any, String](FunctionName("timeofday"))
    val ToTimestamp                 = FunctionDef[Long, ZonedDateTime](FunctionName("to_timestamp"))
    val TransactionTimestamp        = FunctionDef[Any, ZonedDateTime](FunctionName("transaction_timestamp"))
    val Translate                   = FunctionDef[(String, String, String), String](FunctionName("translate"))
    val TrimScale                   = FunctionDef[Double, Double](FunctionName("trim_scale"))
    val Trunc                       = FunctionDef[Double, Double](FunctionName("trunc"))
  }

}
