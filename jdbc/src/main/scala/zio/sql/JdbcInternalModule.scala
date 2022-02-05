package zio.sql

import java.sql._
import java.time.{ OffsetDateTime, OffsetTime, ZoneId, ZoneOffset }

import zio.Chunk

trait JdbcInternalModule { self: Jdbc =>

  private[sql] def extractColumn[A](
    columnIndex: Int,
    resultSet: ResultSet,
    typeTag: TypeTag[A],
    nonNull: Boolean = true
  ): Either[DecodingError, A] = {
    import TypeTag._

    val metaData = resultSet.getMetaData()

    def tryDecode[A](decoder: => Option[A])(implicit expectedType: TypeTag[A]): Either[DecodingError, A] =
      if (resultSet.isClosed()) Left(DecodingError.Closed)
      else {
        val columnExists = columnIndex >= 1 || columnIndex <= metaData.getColumnCount()

        if (!columnExists) Left(DecodingError.MissingColumn(columnIndex))
        else
          try {
            val value = decoder

            value match {
              case Some(value) => Right(value)
              case None        =>
                //TODO following would not be sound - e.g. by outer join any column can be null
                // if (nonNull)
                //   Left(DecodingError.UnexpectedNull(column))
                // else
                Right(null.asInstanceOf[A])
            }
          } catch {
            case _: SQLException =>
              Left(DecodingError.UnexpectedType(expectedType, metaData.getColumnType(columnIndex)))
          }
      }

    typeTag match {

      case TBigDecimal         =>
        // could not do tryDecode[BigDecimal](Option(resultSet.getBigDecimal(columnIndex))) because of
        // suspicious application of an implicit view (math.this.BigDecimal.javaBigDecimal2bigDecimal) in the argument to Option.apply
        val result = resultSet.getBigDecimal(columnIndex)
        if (result == null)
          Right(null.asInstanceOf[A])
        else {
          Right(BigDecimal.javaBigDecimal2bigDecimal(result).asInstanceOf[A])
        }
      case TBoolean            => tryDecode[Boolean](Option(resultSet.getBoolean(columnIndex)))
      case TByte               => tryDecode[Byte](Option(resultSet.getByte(columnIndex)))
      case TByteArray          =>
        tryDecode[Chunk[Byte]](Option(resultSet.getBytes(columnIndex)).map(Chunk.fromArray(_)))
      case TChar               => tryDecode[Char](Option(resultSet.getString(columnIndex)).map(_.charAt(0)))
      case TDouble             => tryDecode[Double](Option(resultSet.getDouble(columnIndex)))
      case TFloat              => tryDecode[Float](Option(resultSet.getFloat(columnIndex)))
      case TInstant            =>
        tryDecode[java.time.Instant](Option(resultSet.getTimestamp(columnIndex)).map(_.toInstant()))
      case TInt                => tryDecode[Int](Option(resultSet.getInt(columnIndex)))
      case TLocalDate          =>
        tryDecode[java.time.LocalDate](
          Option(resultSet.getTimestamp(columnIndex)).map(_.toLocalDateTime().toLocalDate())
        )
      case TLocalDateTime      =>
        tryDecode[java.time.LocalDateTime](Option(resultSet.getTimestamp(columnIndex)).map(_.toLocalDateTime()))
      case TLocalTime          =>
        tryDecode[java.time.LocalTime](
          Option(resultSet.getTimestamp(columnIndex)).map(_.toLocalDateTime().toLocalTime())
        )
      case TLong               => tryDecode[Long](Option(resultSet.getLong(columnIndex)))
      case TOffsetDateTime     =>
        tryDecode[OffsetDateTime](
          Option(resultSet.getTimestamp(columnIndex)).map(_.toLocalDateTime().atOffset(ZoneOffset.UTC))
        )
      case TOffsetTime         =>
        tryDecode[OffsetTime](
          Option(resultSet.getTime(columnIndex)).map(_.toLocalTime.atOffset(ZoneOffset.UTC))
        )
      case TShort              => tryDecode[Short](Option(resultSet.getShort(columnIndex)))
      case TString             => tryDecode[String](Option(resultSet.getString(columnIndex)))
      case TUUID               =>
        tryDecode[java.util.UUID](
          Option(resultSet.getString(columnIndex)).map(java.util.UUID.fromString(_))
        )
      case TZonedDateTime      =>
        //2013-07-15 08:15:23.5+00
        tryDecode[java.time.ZonedDateTime](
          Option(resultSet.getTimestamp(columnIndex))
            .map(_.toInstant())
            .map(instant =>
              java.time.ZonedDateTime
                .ofInstant(
                  instant,
                  ZoneId.of(ZoneOffset.UTC.getId)
                )
            )
        )
      case TDialectSpecific(t) => t.decode(columnIndex, resultSet)
      case t @ Nullable()      =>
        val _ = nonNull
        extractColumn(columnIndex, resultSet, t.typeTag, false).map(Option(_))
    }
  }

  private[sql] def getColumns(read: Read[_]): Vector[TypeTag[_]] =
    read match {
      case Read.Mapped(read, _) => getColumns(read)
      case Read.Subselect(selection, _, _, _, _, _, _, _) =>
        selection.value.selectionsUntyped.toVector.map(_.asInstanceOf[ColumnSelection[_, _]]).map {
          case t @ ColumnSelection.Constant(_, _) => t.typeTag
          case t @ ColumnSelection.Computed(_, _) => t.typeTag
        }
      case Read.Union(left, _, _)                         => getColumns(left)
      case v @ Read.Literal(_)                            => scala.collection.immutable.Vector(v.typeTag)
    }

   private[sql] def unsafeExtractRow[A](
    resultSet: ResultSet,
    schema: Vector[(TypeTag[_], Int)]
  ): Either[DecodingError, A] = {
    val result: Either[DecodingError, List[Any]] = Right(List())

    schema
      .foldRight(result) {
        case (_, err @ Left(_))            => err // TODO: Accumulate errors
        case ((typeTag, index), Right(vs)) =>
          extractColumn(index, resultSet, typeTag) match {
            case Left(err) => Left(err)
            case Right(v)  => Right(v :: vs)
          }
      }
       .map {
        case List(a)                                                                => (a)
        case List(a, b)                                                             => (a, b)
        case List(a, b, c)                                                          => (a, b, c)
        case List(a, b, c, d)                                                       => (a, b, c, d)
        case List(a, b, c, d, e)                                                    => (a, b, c, d, e)
        case List(a, b, c, d, e, f)                                                 => (a, b, c, d, e, f)
        case List(a, b, c, d, e, f, g)                                              => (a, b, c, d, e, f, g)
        case List(a, b, c, d, e, f, g, h)                                           => (a, b, c, d, e, f, g, h)
        case List(a, b, c, d, e, f, g, h, i)                                        => (a, b, c, d, e, f, g, h, i)
        case List(a, b, c, d, e, f, g, h, i, j)                                     => (a, b, c, d, e, f, g, h, i, j)
        case List(a, b, c, d, e, f, g, h, i, j, k)                                  => (a, b, c, d, e, f, g, h, i, j, k)
        case List(a, b, c, d, e, f, g, h, i, j, k, l)                               => (a, b, c, d, e, f, g, h, i, j, k, l)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m)                            => (a, b, c, d, e, f, g, h, i, j, k, l, m)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n)                         => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)                      => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)                   => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)                =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)             =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)          =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)       =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)    =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
        case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
        case _                                                                      => ()
      }
      .map(_.asInstanceOf[A])
  }
}
