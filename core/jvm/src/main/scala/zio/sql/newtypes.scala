package zio.sql

trait NewtypesModule extends NewtypeExports {

  object ColumnName extends Newtype[String]
  type ColumnName = ColumnName.type
  object TableName extends Newtype[String]
  type TableName = TableName.type

  sealed case class FunctionName(name: String)

}

/**
 * a cut back version of zio-prelude's NewTypes module
 * https://github.com/zio/zio-prelude/blob/master/core/shared/src/main/scala/zio/prelude/newtypes/package.scala
 */
private[sql] sealed trait NewtypeModule {

  def newtype[A]: Newtype[A]

  def subtype[A]: Subtype[A]

  private[this] type Id[+A] = A

  sealed trait Newtype[A] {
    type Type

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def apply(value: A): Type = wrap(value)

    /**
     * Allows pattern matching on newtype instances to convert them back to
     * instances of the underlying type.
     */
    def unapply(value: Type): Some[A] = Some(unwrap(value))

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def wrap(value: A): Type = wrapAll[Id](value)

    /**
     * Converts an instance of the newtype back to an instance of the
     * underlying type.
     */
    def unwrap(value: Type): A = unwrapAll[Id](value)

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype. For example,
     * this could be used to convert a list of instances of the underlying
     * type to a list of instances of the newtype.
     */
    def wrapAll[F[_]](value: F[A]): F[Type]

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type. For example,
     * this could be used to convert a list of instances of the newtype back
     * to a list of instances of the underlying type.
     */
    def unwrapAll[F[_]](value: F[Type]): F[A]
  }

  sealed trait Subtype[A] extends Newtype[A] {
    type Type <: A
  }
}

private[sql] object NewtypeModule {
  val instance: NewtypeModule =
    new NewtypeModule {
      def newtype[A]: Newtype[A] =
        new Newtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value

          def unwrapAll[F[_]](value: F[Type]): F[A] = value
        }

      def subtype[A]: Subtype[A] =
        new Subtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value.asInstanceOf[F[Type]]

          def unwrapAll[F[_]](value: F[Type]): F[A] = value.asInstanceOf[F[A]]
        }
    }
}

trait NewtypeExports {
  import NewtypeModule._

  /**
   * The class of objects corresponding to newtypes. Users should implement an
   * object that extends this class to create their own newtypes, specifying
   * `A` as the underlying type to wrap.
   *
   * {{{
   * object Meter extends Newtype[Double]
   * type Meter = Meter.Type
   * }}}
   */
  abstract class Newtype[A] extends instance.Newtype[A] {
    val newtype: instance.Newtype[A] = instance.newtype[A]

    trait Tag extends Any
    type Type = newtype.Type with Tag

    def wrapAll[F[_]](value: F[A]): F[Type] = newtype.wrapAll(value).asInstanceOf[F[Type]]

    def unwrapAll[F[_]](value: F[Type]): F[A] = newtype.unwrapAll(value.asInstanceOf[F[newtype.Type]])
  }

  /**
   * The class of objects corresponding to subtypes. Users should implement an
   * object that extends this class to create their own subtypes, specifying
   * `A` as the underlying type to wrap.
   *
   * {{{
   * object And extends Subtype[Boolean]
   * type And = And.Type
   * }}}
   */
  abstract class Subtype[A] extends instance.Subtype[A] {
    val subtype: instance.Subtype[A] = instance.subtype[A]

    trait Tag extends Any
    type Type = subtype.Type with Tag

    def wrapAll[F[_]](value: F[A]): F[Type] = subtype.wrapAll(value).asInstanceOf[F[Type]]

    def unwrapAll[F[_]](value: F[Type]): F[A] = subtype.unwrapAll(value.asInstanceOf[F[subtype.Type]])
  }
}
