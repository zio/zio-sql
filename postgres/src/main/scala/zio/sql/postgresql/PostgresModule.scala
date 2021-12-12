package zio.sql.postgresql

import org.postgresql.util.PGInterval
import zio.Chunk
import zio.sql.{ Jdbc, Renderer }

import java.sql.ResultSet
import java.text.DecimalFormat
import java.time._
import java.time.format.DateTimeFormatter
import java.util.Calendar
import zio.schema._
import zio.schema.StandardType.BigDecimalType
import zio.schema.StandardType.CharType
import zio.schema.StandardType.IntType
import zio.schema.StandardType.BinaryType
import zio.schema.StandardType.UnitType
import zio.schema.StandardType.DoubleType
import zio.schema.StandardType.BigIntegerType
import zio.schema.StandardType.UUIDType
import zio.schema.StandardType.ShortType
import zio.schema.StandardType.LongType
import zio.schema.StandardType.StringType
import zio.schema.StandardType.BoolType
import zio.schema.StandardType.DayOfWeekType
import zio.schema.StandardType.FloatType

trait PostgresModule extends Jdbc { self =>
  import TypeTag._

  override type TypeTagExtension[+A] = PostgresSpecific.PostgresTypeTag[A]

  override type TableExtension[A] = PostgresSpecific.PostgresSpecificTable[A]

  object PostgresSpecific {
    sealed trait PostgresSpecificTable[A] extends Table.TableEx[A]

    object PostgresSpecificTable {
      import scala.language.implicitConversions

      private[PostgresModule] sealed case class LateraLTable[A, B](left: Table.Aux[A], right: Table.Aux[B])
          extends PostgresSpecificTable[A with B] { self =>

        override type ColumnHead = left.ColumnHead

        override type HeadIdentity0 = left.HeadIdentity0
        override type ColumnTail    =
          left.columnSet.tail.Append[ColumnSet.Cons[right.ColumnHead, right.ColumnTail, right.HeadIdentity0]]

        override val columnSet: ColumnSet.Cons[ColumnHead, ColumnTail, HeadIdentity0] =
          left.columnSet ++ right.columnSet

        override val columnToExpr: ColumnToExpr[A with B] = new ColumnToExpr[A with B] {
          def toExpr[C](column: Column[C]): Expr[Features.Source[column.Identity], A with B, C] =
            if (left.columnSet.contains(column))
              left.columnToExpr.toExpr(column)
            else
              right.columnToExpr.toExpr(column)
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
        override def decode(column: Either[Int, String], resultSet: ResultSet): Either[DecodingError, Unit] =
          scala.util
            .Try(column.fold(resultSet.getObject(_), resultSet.getObject(_)))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              _ => Right(())
            )
      }
      implicit case object TInterval   extends PostgresTypeTag[Interval]   {
        override def decode(
          column: Either[Int, String],
          resultSet: ResultSet
        ): Either[DecodingError, Interval] =
          scala.util
            .Try(Interval.fromPgInterval(new PGInterval(column.fold(resultSet.getString(_), resultSet.getString(_)))))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              r => Right(r)
            )
      }
      implicit case object TTimestampz extends PostgresTypeTag[Timestampz] {
        override def decode(
          column: Either[Int, String],
          resultSet: ResultSet
        ): Either[DecodingError, Timestampz] =
          scala.util
            .Try(
              Timestampz.fromZonedDateTime(
                ZonedDateTime
                  .ofInstant(
                    column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toInstant,
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

    //Based upon https://github.com/tminglei/slick-pg/blob/master/src/main/scala/com/github/tminglei/slickpg/PgDateSupport.scala
    sealed case class Interval(
      years: Int = 0,
      months: Int = 0,
      days: Int = 0,
      hours: Int = 0,
      minutes: Int = 0,
      seconds: BigDecimal = 0.0
    ) {
      private val secondsFormat = {
        val format = new DecimalFormat("0.00####")
        val dfs    = format.getDecimalFormatSymbols()
        dfs.setDecimalSeparator('.')
        format.setDecimalFormatSymbols(dfs)
        format
      }
      def milliseconds: Int = (microseconds + (if (microseconds < 0) -500 else 500)) / 1000
      def microseconds: Int = (seconds * 1000000.0).asInstanceOf[Int]

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
        date.setTime((cal +: this).getTime.getTime)
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

    object LateralTableExample {
      import self.ColumnSet._

      // ColumnSetAspect
      // val nullableUUID = uuid("id") @@ nullable

      val customers =
        (uuid("id") ++ localDate("dob") ++ string("first_name") ++ string("last_name") ++ boolean(
          "verified"
        ) ++ zonedDateTime("created_timestamp"))
          .table("customers")

      val customerId :*: dob :*: fName :*: lName :*: verified :*: createdTimestamp :*: _ =
        customers.columns

      val orders = (uuid("id") ++ uuid("customer_id") ++ localDate("order_date")).table("orders")

      val orderId :*: fkCustomerId :*: orderDate :*: _ = orders.columns

      val products                                     =
        (uuid("id") ++ string("name") ++ string("description") ++ string("image_url")).table("products")
      val productId :*: description :*: imageURL :*: _ = products.columns

      val productPrices                             =
        (uuid("product_id") ++ offsetDateTime("effective") ++ bigDecimal("price")).table("product_prices")
      val fkProductId :*: effective :*: price :*: _ = productPrices.columns

      val orderDetails                                                         =
        (uuid("order_id") ++ uuid("product_id") ++ int("quantity") ++ bigDecimal("unit_price"))
          .table(
            "order_details"
          )
      val fkOrderId :*: orderDetailsProductId :*: quantity :*: unitPrice :*: _ = orderDetails.columns

      // ============= INSERTS

      // val persons5 = table[Person5]("persons5")
      // val name5 :*: age5 :*: gender5 :*: birth5 :*: _ = persons5.columns

      val persons1 = (string("name")).table("persons1")
      val persons2 = (string("name") ++ int("age")).table("persons2")
      val persons3 = (string("name") ++ int("age") ++ string("gender")).table("persons3")
      val persons4 = (string("name") ++ int("age") ++ string("gender") ++ long("date_of_birth")).table("persons4")

      val name1 :*: _                                 = persons1.columns
      val name2 :*: age2 :*: _                        = persons2.columns
      val name3 :*: age3 :*: gender3 :*: _            = persons3.columns
      val name4 :*: age4 :*: gender4 :*: birth4 :*: _ = persons4.columns

      case class Person1(name: String)
      case class Person2(name: String, age: Int)
      case class Person3(name: String, age: Int, gender: String)
      case class Person4(name: String, age: Int, gender: String, dateOfBirth: Long)

      implicit val personSchema1 = DeriveSchema.gen[Person1]
      implicit val personSchema2 = DeriveSchema.gen[Person2]
      implicit val personSchema3 = DeriveSchema.gen[Person3]
      implicit val personSchema4 = DeriveSchema.gen[Person4]

      val personValues1: List[Person1] = ???
      val personValues2: List[Person2] = ???
      val personValues3: List[Person3] = ???
      val personValues4: List[Person4] = ???

      insertInto(persons1)(name1).values(personValues1)
      insertInto(persons2)(name2 ++ age2).values(personValues2)
      insertInto(persons3)(name3 ++ age3 ++ gender3).values(personValues3)

      val personValues1Tuple: List[String]                = ???
      val personValues2Tuple: List[(String, Int)]         = ???

      insertInto(persons1)(name1).values(personValues1Tuple)
      insertInto(persons2)(name2 ++ age2).values(personValues2Tuple)


      val personValues3Tuple: List[(String, Int, String)] = ???
      insertInto(persons3)(name3 ++ age3 ++ gender3).values(personValues3Tuple)

      def test[A, B](expr1: Expr[Features.Source[A], _, _], expr2: Expr[Features.Source[B], _, _])(implicit
        eq: A =:= B
      ) = {
        val _ = expr1
        val _ = expr2
      }

      insertAltInto(customers)
        .values(
          (customerId         -> java.util.UUID.fromString("28e880be-c783-43ea-9839-db51834347a8")) ++
            (dob              -> LocalDate.now()) ++
            (fName            -> "Jaro") ++
            (lName            -> "Regec") ++
            (verified         -> true) ++
            (createdTimestamp -> ZonedDateTime.now())
        )
    }
  }

  object PostgresFunctionDef {
    import PostgresSpecific._

    val SplitPart                   = FunctionDef[(String, String, Int), String](FunctionName("split_part"))
    val IsFinite                    = FunctionDef[Instant, Boolean](FunctionName("isfinite"))
    val TimeOfDay                   = FunctionDef[Any, String](FunctionName("timeofday"))
    val CurrentTime                 = Expr.ParenlessFunctionCall0[OffsetTime](FunctionName("current_time"))
    val CharLength                  = FunctionDef[String, Int](FunctionName("character_length"))
    val Localtime                   = Expr.ParenlessFunctionCall0[LocalTime](FunctionName("localtime"))
    val LocaltimeWithPrecision      = FunctionDef[Int, LocalTime](FunctionName("localtime"))
    val Localtimestamp              = Expr.ParenlessFunctionCall0[Instant](FunctionName("localtimestamp"))
    val LocaltimestampWithPrecision = FunctionDef[Int, Instant](FunctionName("localtimestamp"))
    val Md5                         = FunctionDef[String, String](FunctionName("md5"))
    val ParseIdent                  = FunctionDef[String, String](FunctionName("parse_ident"))
    val Chr                         = FunctionDef[Int, String](FunctionName("chr"))
    val CurrentDate                 = Expr.ParenlessFunctionCall0[LocalDate](FunctionName("current_date"))
    val Initcap                     = FunctionDef[String, String](FunctionName("initcap"))
    val Repeat                      = FunctionDef[(String, Int), String](FunctionName("repeat"))
    val Reverse                     = FunctionDef[String, String](FunctionName("reverse"))
    val TrimScale                   = FunctionDef[Double, Double](FunctionName("trim_scale"))
    val Hex                         = FunctionDef[Int, String](FunctionName("to_hex"))
    val Left                        = FunctionDef[(String, Int), String](FunctionName("left"))
    val Length                      = FunctionDef[String, Int](FunctionName("length"))
    val MinScale                    = FunctionDef[Double, Int](FunctionName("min_scale"))
    val Radians                     = FunctionDef[Double, Double](FunctionName("radians"))
    val Right                       = FunctionDef[(String, Int), String](FunctionName("right"))
    val StartsWith                  = FunctionDef[(String, String), Boolean](FunctionName("starts_with"))
    val Translate                   = FunctionDef[(String, String, String), String](FunctionName("translate"))
    val Trunc                       = FunctionDef[Double, Double](FunctionName("trunc"))
    val Sind                        = FunctionDef[Double, Double](FunctionName("sind"))
    val GCD                         = FunctionDef[(Double, Double), Double](FunctionName("gcd"))
    val LCM                         = FunctionDef[(Double, Double), Double](FunctionName("lcm"))
    val CBRT                        = FunctionDef[Double, Double](FunctionName("cbrt"))
    val Degrees                     = FunctionDef[Double, Double](FunctionName("degrees"))
    val Div                         = FunctionDef[(Double, Double), Double](FunctionName("div"))
    val Factorial                   = FunctionDef[Int, Int](FunctionName("factorial"))
    val Random                      = FunctionDef[Any, Double](FunctionName("random"))
    val LPad                        = FunctionDef[(String, Int, String), String](FunctionName("lpad"))
    val RPad                        = FunctionDef[(String, Int, String), String](FunctionName("rpad"))
    val ToTimestamp                 = FunctionDef[Long, ZonedDateTime](FunctionName("to_timestamp"))
    val PgClientEncoding            = FunctionDef[Any, String](FunctionName("pg_client_encoding"))
    val Now                         = FunctionDef[Any, ZonedDateTime](FunctionName("now"))
    val StatementTimestamp          = FunctionDef[Any, ZonedDateTime](FunctionName("statement_timestamp"))
    val TransactionTimestamp        = FunctionDef[Any, ZonedDateTime](FunctionName("transaction_timestamp"))
    val MakeDate                    = FunctionDef[(Int, Int, Int), LocalDate](FunctionName("make_date"))
    val MakeInterval                = FunctionDef[Interval, Interval](FunctionName("make_interval"))
    val MakeTime                    = FunctionDef[(Int, Int, Double), LocalTime](FunctionName("make_time"))
    val MakeTimestamp               =
      FunctionDef[(Int, Int, Int, Int, Int, Double), LocalDateTime](
        FunctionName("make_timestamp")
      )
    val MakeTimestampz              =
      FunctionDef[Timestampz, Timestampz](FunctionName("make_timestamptz"))
    val Encode                      = FunctionDef[(Chunk[Byte], String), String](FunctionName("encode"))
    val Decode                      = FunctionDef[(String, String), Chunk[Byte]](FunctionName("decode"))
    val Format0                     = FunctionDef[String, String](FunctionName("format")) // TODO: varargs
    val Format1                     = FunctionDef[(String, Any), String](FunctionName("format"))
    val Format2                     = FunctionDef[(String, Any, Any), String](FunctionName("format"))
    val Format3                     = FunctionDef[(String, Any, Any, Any), String](FunctionName("format"))
    val Format4                     = FunctionDef[(String, Any, Any, Any, Any), String](FunctionName("format"))
    val Format5                     = FunctionDef[(String, Any, Any, Any, Any, Any), String](FunctionName("format"))
    val SetSeed                     = FunctionDef[Double, Unit](FunctionName("setseed"))
    val BitLength                   = FunctionDef[String, Int](FunctionName("bit_length"))
    val Pi                          = Expr.FunctionCall0[Double](FunctionDef[Any, Double](FunctionName("pi")))
  }

  override def renderRead(read: self.Read[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderModule.renderReadImpl(read)
    render.toString
  }

  def renderUpdate(update: Update[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderModule.renderUpdateImpl(update)
    render.toString
  }

  override def renderInsertAlt(insert: self.InsertAlt[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderModule.renderInsertAltImpl(insert)
    render.toString
  }

  override def renderInsert[A: Schema](insert: self.Insert[_, A]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderModule.renderInsertImpl(insert)
    render.toString
  }

  override def renderDelete(delete: Delete[_]): String = {
    implicit val render: Renderer = Renderer()
    PostgresRenderModule.renderDeleteImpl(delete)
    render.toString
  }

  object PostgresRenderModule {
    //todo split out to separate module

    def renderInsertImpl[A](insert: Insert[_, A])(implicit render: Renderer, schema: Schema[A]) = {
      render("INSERT INTO ")
      renderTable(insert.table)

      render(" (")
      renderColumnNames(insert.sources)
      render(") VALUES ")

      renderInsertValues(insert.values)
    }

    def renderInsertValues[A](col: Seq[A])(implicit render: Renderer, schema: Schema[A]): Unit =
      //TODO any performance penalty because of toList ?
      col.toList match {
        case head :: Nil  =>
          render("(")
          renderInserValue(head)
          render(");")
        case head :: next =>
          render("(")
          renderInserValue(head)(render, schema)
          render(" ),")
          renderInsertValues(next)
        case Nil          => ()
      }

    def renderInserValue[Z](z: Z)(implicit render: Renderer, schema: Schema[Z]): Unit =
      schema.toDynamic(z) match {
        case DynamicValue.Record(listMap) =>
          listMap.values.toList match {
            case head :: Nil  => renderDynamicValue(head)
            case head :: next =>
              renderDynamicValue(head)
              render(", ")
              renderDynamicValues(next)
            case Nil          => ()
          }
        case value => renderDynamicValue(value)
      }

    def renderDynamicValues(dynValues: List[DynamicValue])(implicit render: Renderer): Unit =
      dynValues match {
        case head :: Nil  => renderDynamicValue(head)
        case head :: tail =>
          renderDynamicValue(head)
          render(", ")
          renderDynamicValues(tail)
        case Nil          => ()
      }

    // TODO render each type according to their specifics & test it
    def renderDynamicValue(dynValue: DynamicValue)(implicit render: Renderer): Unit =
      dynValue match {
        case DynamicValue.Primitive(value, typeTag) =>
          // need to do this since StandardType is invariant in A
          StandardType.fromString(typeTag.tag) match {
            case Some(v) =>
              v match {
                case BigDecimalType                         =>
                  println("foo")
                  render(value)
                case StandardType.Instant(formatter)        => render(s"'${formatter.format(value.asInstanceOf[Instant])}'")
                case CharType                               => render(s"'${value}'")
                case IntType                                => render(value)
                case StandardType.MonthDay                  => render(s"'${value}'")
                case BinaryType                             => render(s"'${value}'")
                case StandardType.Month                     => render(s"'${value}'")
                case StandardType.LocalDateTime(formatter)  =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalDateTime])}'")
                case UnitType                               => () // ???
                case StandardType.YearMonth                 => render(s"'${value}'")
                case DoubleType                             => render(value)
                case StandardType.Year                      => render(s"'${value}'")
                case StandardType.OffsetDateTime(formatter) =>
                  render(s"'${formatter.format(value.asInstanceOf[OffsetDateTime])}'")
                case StandardType.ZonedDateTime(formatter)  =>
                  //render(s"'${formatter.format(value.asInstanceOf[ZonedDateTime])}'")
                  render(s"'${DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[ZonedDateTime])}'")
                case BigIntegerType                         => render(s"'${value}'")
                case UUIDType                               => render(s"'${value}'")
                case StandardType.ZoneOffset                => render(s"'${value}'")
                case ShortType                              => render(value)
                case StandardType.LocalTime(formatter)      =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalTime])}'")
                case StandardType.OffsetTime(formatter)     =>
                  render(s"'${formatter.format(value.asInstanceOf[OffsetTime])}'")
                case LongType                               => render(value)
                case StringType                             => render(s"'${value}'")
                case StandardType.Period                    => render(s"'${value}'")
                case StandardType.ZoneId                    => render(s"'${value}'")
                case StandardType.LocalDate(formatter)      =>
                  render(s"'${formatter.format(value.asInstanceOf[LocalDate])}'")
                case BoolType                               => render(value)
                case DayOfWeekType                          => render(s"'${value}'")
                case FloatType                              => render(value)
                case StandardType.Duration(temporalUnit)    => render(s"'${value}'")
              }
            case None    => ()
          }
        //TODO do we need to handle also other cases?
        case DynamicValue.Transform(that)           => renderDynamicValue(that)
        case DynamicValue.Tuple(left, right)        => {
          renderDynamicValue(left)
          render(", ")
          renderDynamicValue(right)
        }
        case _                                      => ()
      }

    def renderColumnNames(sources: SelectionSet[_])(implicit render: Renderer): Unit =
      sources match {
        case SelectionSet.Empty                                     => () // table is a collection of at least ONE column
        case SelectionSet.Cons(columnSelection, SelectionSet.Empty) =>
          val _ = columnSelection.name.map { name =>
            render(name)
          }
        case SelectionSet.Cons(columnSelection, tail)               =>
          val _ = columnSelection.name.map { name =>
            render(name)
            render(", ")
            renderColumnNames(tail)(render)
          }
      }

    def renderInsertAltImpl(insert: InsertAlt[_])(implicit render: Renderer) = {
      render("INSERT INTO ")
      renderTable(insert.table)

      render(" (")
      renderColumnNamesAlt(insert.values)
      render(") VALUES (")

      renderInsertAltValues(insert.values)
      render(" )")
    }

    def renderInsertAltValues(values: InsertRow[_])(implicit render: Renderer): Unit =
      values match {
        case InsertRow.Empty                            => ()
        case InsertRow.Cons(tupleHead, InsertRow.Empty) =>
          val typeTag = Expr.typeTagOf(tupleHead._1)
          val value   = tupleHead._2
          renderValue(typeTag, value)
        case InsertRow.Cons(tupleHead, tail)            =>
          val typeTag = Expr.typeTagOf(tupleHead._1)
          val value   = tupleHead._2
          renderValue(typeTag, value)
          render(", ")
          renderInsertAltValues(tail)(render)
      }

    /**
     * TODO
     *
     * Every values is written differently - based on Expr typeTag
     */
    def renderValue[A](typeTage: TypeTag[A], value: A)(implicit render: Renderer): Unit =
      typeTage match {
        case TBigDecimal                        => render(value.toString)
        case TBoolean                           => render(value.toString)
        case TByte                              => render(value.toString)
        case TByteArray                         => render(value.toString)
        case TChar                              => render(value.toString)
        case TDouble                            => render(value.toString)
        case TFloat                             => render(value.toString)
        case TInstant                           => render(value.toString)
        case TInt                               => render(value.toString)
        case TLocalDate                         => render(s"'${value.toString}'")
        case TLocalDateTime                     => render(value.toString)
        case TLocalTime                         => render(s"'${value.toString}'")
        case TLong                              => render(value.toString)
        case TOffsetDateTime                    => render(value.toString)
        case TOffsetTime                        => render(value.toString)
        case TShort                             => render(value.toString)
        case TString                            => render(s"'${value.toString}'")
        case TUUID                              => render(s"'${value.toString}'")
        case TZonedDateTime                     =>
          render(s"'${DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(value.asInstanceOf[ZonedDateTime])}'")
        case TDialectSpecific(typeTagExtension) => render(value.toString)
        case Nullable()                         => render("null")
      }

    def renderColumnNamesAlt(values: InsertRow[_])(implicit render: Renderer): Unit =
      values match {
        case InsertRow.Empty                            => () // table is a collection of at least ONE column
        case InsertRow.Cons(tupleHead, InsertRow.Empty) =>
          val expr       = tupleHead._1
          val columnName = expr match {
            case Expr.Source(_, c) => c.name
            case _                 => None
          }
          val _          = columnName.map { name =>
            render(name)
          }
        case InsertRow.Cons(tupleHead, tail)            =>
          val expr       = tupleHead._1
          val columnName = expr match {
            case Expr.Source(_, c) => c.name
            case _                 => None
          }
          val _          = columnName.map { name =>
            render(name)
            render(", ")
            renderColumnNamesAlt(tail)(render)
          }
      }

    def renderDeleteImpl(delete: Delete[_])(implicit render: Renderer) = {
      render("DELETE FROM ")
      renderTable(delete.table)
      delete.whereExpr match {
        case Expr.Literal(true) => ()
        case _                  =>
          render(" WHERE ")
          renderExpr(delete.whereExpr)
      }
    }

    def renderUpdateImpl(update: Update[_])(implicit render: Renderer) =
      update match {
        case Update(table, set, whereExpr) =>
          render("UPDATE ")
          renderTable(table)
          render(" SET ")
          renderSet(set)
          render(" WHERE ")
          renderExpr(whereExpr)
      }

    def renderSet(set: List[Set[_, _]])(implicit render: Renderer): Unit =
      set match {
        case head :: tail =>
          renderExpr(head.lhs)
          render(" = ")
          renderExpr(head.rhs)
          tail.foreach { setEq =>
            render(", ")
            renderExpr(setEq.lhs)
            render(" = ")
            renderExpr(setEq.rhs)
          }
        case Nil          => //TODO restrict Update to not allow empty set
      }

    private[zio] def renderLit[A, B](lit: self.Expr.Literal[_])(implicit render: Renderer): Unit = {
      import PostgresSpecific.PostgresTypeTag._
      import TypeTag._
      lit.typeTag match {
        case TDialectSpecific(tt) =>
          tt match {
            case tt @ TInterval                         => render(tt.cast(lit.value))
            case tt @ TTimestampz                       => render(tt.cast(lit.value))
            case _: PostgresSpecific.PostgresTypeTag[_] => ???
          }
        case TByteArray           =>
          render(
            lit.value.asInstanceOf[Chunk[Byte]].map("""\%03o""" format _).mkString("E\'", "", "\'")
          ) // todo fix `cast` infers correctly but map doesn't work for some reason
        case tt @ TChar           =>
          render("'", tt.cast(lit.value), "'") //todo is this the same as a string? fix escaping
        case tt @ TInstant        => render("TIMESTAMP '", tt.cast(lit.value), "'")
        case tt @ TLocalDate      => render("DATE '", tt.cast(lit.value), "'")
        case tt @ TLocalDateTime  => render("DATE '", tt.cast(lit.value), "'")
        case tt @ TLocalTime      => render(tt.cast(lit.value)) // todo still broken
        case tt @ TOffsetDateTime => render("DATE '", tt.cast(lit.value), "'")
        case tt @ TOffsetTime     => render(tt.cast(lit.value)) // todo still broken
        case tt @ TUUID           => render("'", tt.cast(lit.value), "'")
        case tt @ TZonedDateTime  => render("DATE '", tt.cast(lit.value), "'")

        case TByte       => render(lit.value)           //default toString is probably ok
        case TBigDecimal => render(lit.value)           //default toString is probably ok
        case TBoolean    => render(lit.value)           //default toString is probably ok
        case TDouble     => render(lit.value)           //default toString is probably ok
        case TFloat      => render(lit.value)           //default toString is probably ok
        case TInt        => render(lit.value)           //default toString is probably ok
        case TLong       => render(lit.value)           //default toString is probably ok
        case TShort      => render(lit.value)           //default toString is probably ok
        case TString     => render("'", lit.value, "'") //todo fix escaping
        case _           => render(lit.value)           //todo fix add TypeTag.Nullable[_] =>
      }
    }

    private[zio] def renderExpr[A, B](expr: self.Expr[_, A, B])(implicit render: Renderer): Unit = expr match {
      case Expr.Subselect(subselect)                                                    =>
        render(" (")
        render(renderRead(subselect))
        render(") ")
      case Expr.Source(table, column)                                                   =>
        (table, column.name) match {
          case (tableName: TableName, Some(columnName)) =>
            render(tableName, ".", columnName)
          case _                                        => ()
        }
      case Expr.Unary(base, op)                                                         =>
        render(" ", op.symbol)
        renderExpr(base)
      case Expr.Property(base, op)                                                      =>
        renderExpr(base)
        render(" ", op.symbol)
      case Expr.Binary(left, right, op)                                                 =>
        renderExpr(left)
        render(" ", op.symbol, " ")
        renderExpr(right)
      case Expr.Relational(left, right, op)                                             =>
        renderExpr(left)
        render(" ", op.symbol, " ")
        renderExpr(right)
      case Expr.In(value, set)                                                          =>
        renderExpr(value)
        renderReadImpl(set)
      case lit: Expr.Literal[_]                                                         => renderLit(lit)
      case Expr.AggregationCall(p, aggregation)                                         =>
        render(aggregation.name.name, "(")
        renderExpr(p)
        render(")")
      case Expr.ParenlessFunctionCall0(fn)                                              =>
        val _ = render(fn.name)
      case Expr.FunctionCall0(fn)                                                       =>
        render(fn.name.name)
        render("(")
        val _ = render(")")
      case Expr.FunctionCall1(p, fn)                                                    =>
        render(fn.name.name, "(")
        renderExpr(p)
        render(")")
      case Expr.FunctionCall2(p1, p2, fn)                                               =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(")")
      case Expr.FunctionCall3(p1, p2, p3, fn)                                           =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(",")
        renderExpr(p3)
        render(")")
      case Expr.FunctionCall4(p1, p2, p3, p4, fn)                                       =>
        render(fn.name.name, "(")
        renderExpr(p1)
        render(",")
        renderExpr(p2)
        render(",")
        renderExpr(p3)
        render(",")
        renderExpr(p4)
        render(")")
      case Expr.FunctionCall5(param1, param2, param3, param4, param5, function)         =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(")")
      case Expr.FunctionCall6(param1, param2, param3, param4, param5, param6, function) =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(",")
        renderExpr(param6)
        render(")")
      case Expr.FunctionCall7(
            param1,
            param2,
            param3,
            param4,
            param5,
            param6,
            param7,
            function
          ) =>
        render(function.name.name)
        render("(")
        renderExpr(param1)
        render(",")
        renderExpr(param2)
        render(",")
        renderExpr(param3)
        render(",")
        renderExpr(param4)
        render(",")
        renderExpr(param5)
        render(",")
        renderExpr(param6)
        render(",")
        renderExpr(param7)
        render(")")
    }

    private[zio] def renderReadImpl(read: self.Read[_])(implicit render: Renderer): Unit =
      read match {
        case Read.Mapped(read, _)                           => renderReadImpl(read)
        case read0 @ Read.Subselect(_, _, _, _, _, _, _, _) =>
          object Dummy {
            type F
            type Repr
            type Source
            type Head
            type Tail <: SelectionSet[Source]
          }
          val read =
            read0.asInstanceOf[Read.Subselect[Dummy.F, Dummy.Repr, Dummy.Source, Dummy.Source, Dummy.Head, Dummy.Tail]]
          import read._

          render("SELECT ")
          renderSelection(selection.value)
          table.foreach { t =>
            render(" FROM ")
            renderTable(t)
          }
          whereExpr match {
            case Expr.Literal(true) => ()
            case _                  =>
              render(" WHERE ")
              renderExpr(whereExpr)
          }
          groupBy match {
            case _ :: _ =>
              render(" GROUP BY ")
              renderExprList(groupBy)

              havingExpr match {
                case Expr.Literal(true) => ()
                case _                  =>
                  render(" HAVING ")
                  renderExpr(havingExpr)
              }
            case Nil    => ()
          }
          orderBy match {
            case _ :: _ =>
              render(" ORDER BY ")
              renderOrderingList(orderBy)
            case Nil    => ()
          }
          limit match {
            case Some(limit) => render(" LIMIT ", limit)
            case None        => ()
          }
          offset match {
            case Some(offset) => render(" OFFSET ", offset)
            case None         => ()
          }

        case Read.Union(left, right, distinct) =>
          renderReadImpl(left)
          render(" UNION ")
          if (!distinct) render("ALL ")
          renderReadImpl(right)

        case Read.Literal(values) =>
          render(" (", values.mkString(","), ") ") //todo fix needs escaping
      }

    def renderExprList(expr: List[Expr[_, _, _]])(implicit render: Renderer): Unit =
      expr match {
        case head :: tail =>
          renderExpr(head)
          tail match {
            case _ :: _ =>
              render(", ")
              renderExprList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def renderOrderingList(expr: List[Ordering[Expr[_, _, _]]])(implicit render: Renderer): Unit =
      expr match {
        case head :: tail =>
          head match {
            case Ordering.Asc(value)  => renderExpr(value)
            case Ordering.Desc(value) =>
              renderExpr(value)
              render(" DESC")
          }
          tail match {
            case _ :: _ =>
              render(", ")
              renderOrderingList(tail)
            case Nil    => ()
          }
        case Nil          => ()
      }

    def renderSelection[A](selectionSet: SelectionSet[A])(implicit render: Renderer): Unit =
      selectionSet match {
        case cons0 @ SelectionSet.Cons(_, _) =>
          object Dummy {
            type Source
            type A
            type B <: SelectionSet[Source]
          }
          val cons = cons0.asInstanceOf[SelectionSet.Cons[Dummy.Source, Dummy.A, Dummy.B]]
          import cons._
          renderColumnSelection(head)
          if (tail != SelectionSet.Empty) {
            render(", ")
            renderSelection(tail)
          }
        case SelectionSet.Empty              => ()
      }

    def renderColumnSelection[A, B](columnSelection: ColumnSelection[A, B])(implicit render: Renderer): Unit =
      columnSelection match {
        case ColumnSelection.Constant(value, name) =>
          render(value) //todo fix escaping
          name match {
            case Some(name) => render(" AS ", name)
            case None       => ()
          }
        case ColumnSelection.Computed(expr, name)  =>
          renderExpr(expr)
          name match {
            case Some(name) =>
              Expr.exprName(expr) match {
                case Some(sourceName) if name != sourceName => render(" AS ", name)
                case _                                      => ()
              }
            case _          => () //todo what do we do if we don't have a name?
          }
      }

    def renderTable(table: Table)(implicit render: Renderer): Unit =
      table match {
        case Table.DialectSpecificTable(tableExtension) =>
          tableExtension match {
            case PostgresSpecific.PostgresSpecificTable.LateraLTable(left, derivedTable) =>
              renderTable(left)

              render(" ,lateral ")

              renderTable(derivedTable)
          }
        //The outer reference in this type test cannot be checked at run time?!
        case sourceTable: self.Table.Source             => render(sourceTable.name)
        case Table.DerivedTable(read, name)             =>
          render(" ( ")
          render(renderRead(read.asInstanceOf[Read[_]]))
          render(" ) ")
          render(name)
        case Table.Joined(joinType, left, right, on)    =>
          renderTable(left)
          render(joinType match {
            case JoinType.Inner      => " INNER JOIN "
            case JoinType.LeftOuter  => " LEFT JOIN "
            case JoinType.RightOuter => " RIGHT JOIN "
            case JoinType.FullOuter  => " OUTER JOIN "
          })
          renderTable(right)
          render(" ON ")
          renderExpr(on)
          render(" ")
      }
  }
}
