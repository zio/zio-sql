package zio.sql

import java.sql._
import java.time.{ OffsetDateTime, OffsetTime, ZoneId, ZoneOffset }

import zio.Chunk

trait JdbcInternalModule { self: Jdbc =>
  // TODO: Only support indexes!
  private[sql] def extractColumn[A](
    column: Either[Int, String],
    resultSet: ResultSet,
    typeTag: TypeTag[A],
    nonNull: Boolean = true
  ): Either[DecodingError, A] = {
    import TypeTag._

    val metaData = resultSet.getMetaData()

    def tryDecode[A](decoder: => A)(implicit expectedType: TypeTag[A]): Either[DecodingError, A] =
      if (resultSet.isClosed()) Left(DecodingError.Closed)
      else {
        val columnExists =
          column.fold(
            index => index >= 1 || index <= metaData.getColumnCount(),
            name =>
              try {
                val _ = resultSet.findColumn(name)
                true
              } catch { case _: SQLException => false }
          )

        if (!columnExists) Left(DecodingError.MissingColumn(column))
        else
          try {
            val value = decoder

            if (value == null && nonNull) Left(DecodingError.UnexpectedNull(column))
            else Right(value)
          } catch {
            case _: SQLException =>
              lazy val columnNames = (1 to metaData.getColumnCount()).toVector.map(metaData.getColumnName(_))
              val columnIndex      = column.fold(index => index, name => columnNames.indexOf(name) + 1)

              Left(DecodingError.UnexpectedType(expectedType, metaData.getColumnType(columnIndex)))
          }
      }

    typeTag match {
      case TBigDecimal         => tryDecode[BigDecimal](column.fold(resultSet.getBigDecimal(_), resultSet.getBigDecimal(_)))
      case TBoolean            => tryDecode[Boolean](column.fold(resultSet.getBoolean(_), resultSet.getBoolean(_)))
      case TByte               => tryDecode[Byte](column.fold(resultSet.getByte(_), resultSet.getByte(_)))
      case TByteArray          =>
        tryDecode[Chunk[Byte]](Chunk.fromArray(column.fold(resultSet.getBytes(_), resultSet.getBytes(_))))
      case TChar               => tryDecode[Char](column.fold(resultSet.getString(_), resultSet.getString(_)).charAt(0))
      case TDouble             => tryDecode[Double](column.fold(resultSet.getDouble(_), resultSet.getDouble(_)))
      case TFloat              => tryDecode[Float](column.fold(resultSet.getFloat(_), resultSet.getFloat(_)))
      case TInstant            =>
        tryDecode[java.time.Instant](column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toInstant())
      case TInt                => tryDecode[Int](column.fold(resultSet.getInt(_), resultSet.getInt(_)))
      case TLocalDate          =>
        tryDecode[java.time.LocalDate](
          column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime().toLocalDate()
        )
      case TLocalDateTime      =>
        tryDecode[java.time.LocalDateTime](
          column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime()
        )
      case TLocalTime          =>
        tryDecode[java.time.LocalTime](
          column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime().toLocalTime()
        )
      case TLong               => tryDecode[Long](column.fold(resultSet.getLong(_), resultSet.getLong(_)))
      case TOffsetDateTime     =>
        tryDecode[OffsetDateTime](
          column
            .fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_))
            .toLocalDateTime()
            .atOffset(ZoneOffset.UTC)
        )
      case TOffsetTime         =>
        tryDecode[OffsetTime](
          column
            .fold(resultSet.getTime(_), resultSet.getTime(_))
            .toLocalTime
            .atOffset(ZoneOffset.UTC)
        )
      case TShort              => tryDecode[Short](column.fold(resultSet.getShort(_), resultSet.getShort(_)))
      case TString             => tryDecode[String](column.fold(resultSet.getString(_), resultSet.getString(_)))
      case TUUID               =>
        tryDecode[java.util.UUID](
          java.util.UUID.fromString(column.fold(resultSet.getString(_), resultSet.getString(_)))
        )
      case TZonedDateTime      =>
        //2013-07-15 08:15:23.5+00
        tryDecode[java.time.ZonedDateTime](
          java.time.ZonedDateTime
            .ofInstant(
              column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toInstant,
              ZoneId.of(ZoneOffset.UTC.getId)
            )
        )
      case TDialectSpecific(t) => t.decode(column, resultSet)
      case t @ Nullable()      => extractColumn(column, resultSet, t.typeTag, false).map(Option(_))
    }
  }

  private[sql] def getColumns(read: Read[_]): Vector[TypeTag[_]] =
    read match {
      case Read.Mapped(read, _) => getColumns(read)

      case Read.Select(selection, _, _, _, _, _, _, _) =>
        selection.value.selectionsUntyped.toVector.map(_.asInstanceOf[ColumnSelection[_, _]]).map {
          case t @ ColumnSelection.Constant(_, _) => t.typeTag
          case t @ ColumnSelection.Computed(_, _) => t.typeTag
        }
      case Read.Union(left, _, _)                      => getColumns(left)
      case v @ Read.Literal(_)                         => Vector(v.typeTag)
    }

  private[sql] def unsafeExtractRow[A](
    resultSet: ResultSet,
    schema: Vector[(TypeTag[_], Int)]
  ): Either[DecodingError, A] = {
    val result: Either[DecodingError, Any] = Right(())

    schema
      .foldRight(result) {
        case (_, err @ Left(_))            => err // TODO: Accumulate errors
        case ((typeTag, index), Right(vs)) =>
          extractColumn(Left(index), resultSet, typeTag) match {
            case Left(err) => Left(err)
            case Right(v)  => Right((v, vs))
          }
      }
      .map(_.asInstanceOf[A])
  }
}
