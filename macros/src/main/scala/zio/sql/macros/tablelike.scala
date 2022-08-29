package zio.sql.macros

import scala.reflect.macros.blackbox
import java.time._
import java.util.UUID
import zio.Chunk

sealed trait TableSchema[T] 

object TableSchema {  
  
  final case class Compatible[T]() extends TableSchema[T]

  implicit def materializeTableSchema[T]: TableSchema[T] = macro materializeTableSchemaImpl[T]

  def materializeTableSchemaImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[TableSchema[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val sqlPrimitives = 
      Seq(
        typeOf[java.math.BigDecimal],
        typeOf[BigDecimal],
        typeOf[Boolean],
        typeOf[Byte],
        typeOf[Chunk[Byte]],
        typeOf[Char],
        typeOf[Double],
        typeOf[Float],
        typeOf[Instant],
        typeOf[Int],
        typeOf[LocalDate],
        typeOf[LocalDateTime],
        typeOf[OffsetTime],
        typeOf[LocalTime],
        typeOf[Long],
        typeOf[OffsetDateTime],
        typeOf[Short],
        typeOf[String],
        typeOf[UUID],
        typeOf[ZonedDateTime]
      )

    def isSqlPrimitive(tpe: Type): Boolean = sqlPrimitives.contains(tpe)

    val membrs = tpe.decls.sorted.collect {
      case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
    }.map(_.typeSignature)

    def isSupportedOption(tpe: Type) : Boolean = {
      tpe match {
        case TypeRef(_, hkt, t) => (hkt == symbolOf[Option[_]]) && sqlPrimitives.contains(t.head)
        case _ => false
      }
    }

    val uncompatible =
      membrs
        .filter(m => !isSqlPrimitive(m))
        .filter(t => !isSupportedOption(t))

    if (!tpe.typeSymbol.asClass.isCaseClass) {
      c.abort(
        c.enclosingPosition,
        s"You can only define table with case class"
      )
    } else {
      if (uncompatible.isEmpty)
        c.Expr[TableSchema[T]](q"new zio.sql.macros.TableSchema.Compatible[${q"$tpe"}]()")
      else {
        c.abort(
          c.enclosingPosition,
          s"Unsupported types by SQL ${uncompatible.map(_.dealias.toString()).mkString(", ")}"
        )
      }
    }
  }
}


