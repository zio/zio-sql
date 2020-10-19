package zio.sql

import java.sql._

import java.io.IOException

import zio.{ Chunk, Has, IO, Managed, ZIO, ZLayer, ZManaged }
import zio.blocking.Blocking
import zio.stream.{ Stream, ZStream }

trait Jdbc extends Sql {
  type ConnectionPool = Has[ConnectionPool.Service]
  object ConnectionPool {
    sealed case class Config(url: String, properties: java.util.Properties)

    trait Service {
      def connection: Managed[Exception, Connection]
    }

    val live: ZLayer[Has[Config] with Blocking, IOException, ConnectionPool] =
      ZLayer.fromFunctionManaged[Has[Config] with Blocking, IOException, Service] { env =>
        val blocking = env.get[Blocking.Service]
        val config   = env.get[Config]

        // TODO: Make a real connection pool with warmup.
        ZManaged.succeed {
          new Service {
            // TODO: effectBlockingIO should not require Blocking!!
            def connection: Managed[Exception, Connection] =
              Managed.make(
                blocking
                  .effectBlocking(DriverManager.getConnection(config.url, config.properties))
                  .refineToOrDie[IOException]
              )(conn => blocking.effectBlocking(conn.close()).orDie)
          }
        }
      }
  }

  type DeleteExecutor = Has[DeleteExecutor.Service]
  object DeleteExecutor {
    trait Service {
      def execute(delete: Delete[_, _]): IO[Exception, Unit]
    }
  }

  type UpdateExecutor = Has[UpdateExecutor.Service]
  object UpdateExecutor {
    trait Service {
      def execute(Update: Update[_]): IO[Exception, Long]
    }
  }

  type ReadExecutor = Has[ReadExecutor.Service]
  object ReadExecutor {
    trait Service {
      def execute[A <: SelectionSet[_], Target](read: Read[A])(to: read.ResultType => Target): Stream[Exception, Target]
    }

    val live: ZLayer[ConnectionPool with Blocking, Nothing, ReadExecutor] =
      ZLayer.fromServices[ConnectionPool.Service, Blocking.Service, ReadExecutor.Service] { (pool, blocking) =>
        new Service {
          // TODO: Allow partial success for nullable columns
          def execute[A <: SelectionSet[_], Target](
            read: Read[A]
          )(to: read.ResultType => Target): Stream[Exception, Target] =
            ZStream
              .managed(pool.connection)
              .flatMap(conn =>
                Stream.unwrap {
                  blocking.effectBlocking {
                    val schema = getColumns(read).zipWithIndex.map { case (value, index) => (value, index + 1) } // SQL is 1-based indexing

                    val query = renderRead(read)

                    val statement = conn.createStatement()

                    val _ = statement.execute(query) // TODO: Check boolean return value

                    val resultSet = statement.getResultSet()

                    ZStream.fromEffectOption {
                      blocking.blocking {
                        ZIO.effectTotal {
                          if (resultSet.next()) {
                            try {
                              unsafeExtractRow[read.ResultType](resultSet, schema) match {
                                case Left(error)  => ZIO.fail(Some(error))
                                case Right(value) => ZIO.succeed(to(value))
                              }
                            } catch {
                              case e: SQLException => ZIO.fail(Some(e))
                            }
                          } else ZIO.fail(None)
                        }
                      }.flatten
                    }
                  }.refineToOrDie[Exception]
                }
              )
        }
      }

    sealed trait DecodingError extends Exception {
      def message: String
    }
    object DecodingError {
      sealed case class UnexpectedNull(column: Either[Int, String]) extends DecodingError {
        private def label = column.fold(index => index.toString, name => name)

        def message = s"Expected column ${label} to be non-null"
      }
      sealed case class UnexpectedType(expected: TypeTag[_], actual: Int) extends DecodingError {
        def message = s"Expected type ${expected} but found ${actual}"
      }
      sealed case class MissingColumn(column: Either[Int, String]) extends DecodingError {
        private def label = column.fold(index => index.toString, name => name)

        def message = s"The column ${label} does not exist"
      }
      case object Closed extends DecodingError {
        def message = s"The ResultSet has been closed, so decoding is impossible"
      }
    }

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
        case TBigDecimal => tryDecode[BigDecimal](column.fold(resultSet.getBigDecimal(_), resultSet.getBigDecimal(_)))
        case TBoolean    => tryDecode[Boolean](column.fold(resultSet.getBoolean(_), resultSet.getBoolean(_)))
        case TByte       => tryDecode[Byte](column.fold(resultSet.getByte(_), resultSet.getByte(_)))
        case TByteArray =>
          tryDecode[Chunk[Byte]](Chunk.fromArray(column.fold(resultSet.getBytes(_), resultSet.getBytes(_))))
        case TChar   => tryDecode[Char](column.fold(resultSet.getString(_), resultSet.getString(_)).charAt(0))
        case TDouble => tryDecode[Double](column.fold(resultSet.getDouble(_), resultSet.getDouble(_)))
        case TFloat  => tryDecode[Float](column.fold(resultSet.getFloat(_), resultSet.getFloat(_)))
        case TInstant =>
          tryDecode[java.time.Instant](column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toInstant())
        case TInt => tryDecode[Int](column.fold(resultSet.getInt(_), resultSet.getInt(_)))
        case TLocalDate =>
          tryDecode[java.time.LocalDate](
            column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime().toLocalDate()
          )
        case TLocalDateTime =>
          tryDecode[java.time.LocalDateTime](
            column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime()
          )
        case TLocalTime =>
          tryDecode[java.time.LocalTime](
            column.fold(resultSet.getTimestamp(_), resultSet.getTimestamp(_)).toLocalDateTime().toLocalTime()
          )
        case TLong           => tryDecode[Long](column.fold(resultSet.getLong(_), resultSet.getLong(_)))
        case TOffsetDateTime => ???
        case TOffsetTime     => ???
        case TShort          => tryDecode[Short](column.fold(resultSet.getShort(_), resultSet.getShort(_)))
        case TString         => tryDecode[String](column.fold(resultSet.getString(_), resultSet.getString(_)))
        case TUUID =>
          tryDecode[java.util.UUID](
            java.util.UUID.fromString(column.fold(resultSet.getString(_), resultSet.getString(_)))
          )
        case TZonedDateTime      => ???
        case TDialectSpecific(_) => ???
        case t @ Nullable()      => extractColumn(column, resultSet, t.typeTag, false).map(Option(_))
      }
    }

    private[sql] def getColumns(read: Read[_]): Vector[TypeTag[_]] =
      read match {
        case Read.Select(selection, _, _, _, _, _, _, _) =>
          selection.value.selectionsUntyped.toVector.map {
            case t @ ColumnSelection.Constant(_, _) => t.typeTag
            case t @ ColumnSelection.Computed(_, _) => t.typeTag
          }
        case Read.Union(left, _, _) => getColumns(left)
        case v @ Read.Literal(_)    => Vector(v.typeTag)
      }

    private[sql] def unsafeExtractRow[A](
      resultSet: ResultSet,
      schema: Vector[(TypeTag[_], Int)]
    ): Either[DecodingError, A] = {
      val result: Either[DecodingError, Any] = Right(())

      schema
        .foldRight(result) {
          case (_, err @ Left(_)) => err // TODO: Accumulate errors
          case ((typeTag, index), Right(vs)) =>
            extractColumn(Left(index), resultSet, typeTag) match {
              case Left(err) => Left(err)
              case Right(v)  => Right((v, vs))
            }
        }
        .map(_.asInstanceOf[A])
    }
  }

  def execute[A <: SelectionSet[_]](read: Read[A]): ExecuteBuilder[A, read.ResultType] = new ExecuteBuilder(read)

  class ExecuteBuilder[Set <: SelectionSet[_], Output](val read: Read.Aux[Output, Set]) {
    import zio.stream._

    def to[A, Target](f: A => Target)(implicit ev: Output <:< (A, Unit)): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, _) = ev(resultType)

        f(a)
      }))

    def to[A, B, Target](
      f: (A, B) => Target
    )(implicit ev: Output <:< (A, (B, Unit))): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, _)) = ev(resultType)

        f(a, b)
      }))

    def to[A, B, C, D, Target](
      f: (A, B, C, D) => Target
    )(implicit ev: Output <:< (A, (B, (C, (D, Unit))))): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, _)))) = ev(resultType)

        f(a, b, c, d)
      }))

    def to[A, B, C, D, E, Target](
      f: (A, B, C, D, E) => Target
    )(implicit ev: Output <:< (A, (B, (C, (D, (E, Unit)))))): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, _))))) = ev(resultType)

        f(a, b, c, d, e)
      }))

    def to[A, B, C, D, E, F, G, H, Target](
      f: (A, B, C, D, E, F, G, H) => Target
    )(implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, Unit))))))))): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, _)))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h)
      }))

    def to[A, B, C, D, E, F, G, H, I, Target](
      f: (A, B, C, D, E, F, G, H, I) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, Unit)))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, _))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, Unit))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, _)))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, Unit)))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, _))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, Unit))))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, _)))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, Unit)))))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, _))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Unit))))))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, _)))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Target
    )(
      implicit ev: Output <:< (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, Unit)))))))))))))))))
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, _))))))))))))))))) = ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Target
    )(
      implicit ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, Unit)))))))))))))))))
      )
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, _)))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Target
    )(
      implicit ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, Unit))))))))))))))))))
      )
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, _))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s)
      }))

    def to[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Target](
      f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Target
    )(
      implicit ev: Output <:< (
        A,
        (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, Unit)))))))))))))))))))
      )
    ): ZStream[ReadExecutor, Exception, Target] =
      ZStream.unwrap(ZIO.access[ReadExecutor](_.get.execute(read) { resultType =>
        val (a, (b, (c, (d, (e, (fArg, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, (t, _)))))))))))))))))))) =
          ev(resultType)

        f(a, b, c, d, e, fArg, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      }))
  }
}
