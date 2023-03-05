package zio.sql.table

import zio.sql.typetag.TypeTag

sealed trait Column[+A] {
  type Identity
  def typeTag: TypeTag[A]

  def name: Option[String]

  def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity]
}

object Column {

  type Aux[+A0, Identity0] = Column[A0] {
    type Identity = Identity0
  }

  final case class Named[A: TypeTag, ColumnIdentity](columnName: String) extends Column[A] {
    override type Identity = ColumnIdentity

    override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

    override def name = Some(columnName)

    override def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity] =
      Column.Named[Option[A1], ColumnIdentity](columnName)
  }

  final case class Indexed[A: TypeTag, ColumnIdentity]() extends Column[A] {

    override type Identity = ColumnIdentity

    override def typeTag: TypeTag[A] = implicitly[TypeTag[A]]

    override def name = None

    override def nullable[A1 >: A](implicit ev: TypeTag.NotNull[A1]): Column.Aux[Option[A1], Identity] =
      Column.Indexed[Option[A1], ColumnIdentity]()
  }

  type Untyped = Column[_]
}
