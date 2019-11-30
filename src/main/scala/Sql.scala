package zio.sql

object Sql {
  type ColumnName = String
  type TableName

  sealed trait ColumnSet {
    type Repr[F[_]]
  }

  sealed trait CNil extends ColumnSet {
    type Repr[F[_]] = F[Unit]
  }
  sealed trait :*:[A, B <: ColumnSet] extends ColumnSet {
    type Repr[F[_]] = (F[A], B#Repr[F])
  }

  final case class Column[A](name: String)
  object Column {
    def int(name: String): Column[Int] = Column[Int](name)
    def long(name: String): Column[Long] = Column[Long](name)
    def string(name: String): Column[String] = Column[String](name)
  }

  sealed trait ColumnSchema[A <: ColumnSet] { self =>
    final type Expr1[X] = Table[A] => Expr[A, X]

    final def :*: [B](column: Column[B]): ColumnSchema[B :*: A] = new ColumnSchema[B :*: A] {
      def fields = (table => Expr.Source(table, column), self.fields.asInstanceOf[A#Repr[Expr1]])

      private[Sql] def flatten: List[Column[_]] = column :: self.flatten
    }

    def fields: A#Repr[Expr1]

    private[Sql] def flatten: List[Column[_]]
  }
  object ColumnSchema {
    val empty: ColumnSchema[CNil] = new ColumnSchema[CNil] {
      def fields = _ => Expr.Unit

      private[Sql] def flatten: List[Column[_]] = Nil
    }
  }    

  object :*: {
    def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
  }
  object Person {
    import Column._ 

    val person = string("name") :*: int("age") :*: ColumnSchema.empty

    val name :*: age :*: _ = person.fields
  }

  sealed trait Table[A <: ColumnSet] { self =>
    def apply[B](f: Table[A] => Column[B]): Expr[A, B] = Expr.Source(self, f(self))
  }
  object Table {
    final case class Source[A <: ColumnSet](name: TableName, ColumnSchema: ColumnSchema[A]) extends Table[A]
  }

  /**
   * (SELECT *, "foo", table.a + table.b AS sum... FROM table WHERE cond) UNION (SELECT ... FROM table)
   *   UNION ('1', '2', '3')
   * UPDATE table SET ...
   * INSERT ... INTO table
   * DELETE ... FROM table
   */
  sealed trait Sql[-A, +B]

  /**
   * A `Read[A]` models a selection of a set of values of type `A`.
   */
  sealed trait Read[+A]
  object Read {
    final case class Select[A <: ColumnSet, B](
      selection: Selection[A, B], table: Table[A], where: Where[A]) extends Read[B]
    
    final case class Union[B](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B]

    final case class Literal[B](values: Iterable[B]) extends Read[B]
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed trait Selection[-A, +B]
  object Selection {
    final case class Identity[A]() extends Selection[A, A]
    final case class Constant[A](value: A) extends Selection[Any, A]
    final case class Concat[A, L, R](left: Selection[A, L], right: Selection[A, R]) 
      extends Selection[A, (L, R)]
    final case class Computed[A, B](expr: Expr[A, B], name: Option[ColumnName]) extends 
      Selection[A, B]
  }

  sealed trait BinaryOp[A]
  object BinaryOp {
    case object AddInt extends BinaryOp[Int]
    case object MultInt extends BinaryOp[Int]
    case object AddLong extends BinaryOp[Long]
    case object MultLong extends BinaryOp[Long]
    case object StringConcat extends BinaryOp[String]
  }

  /**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
  sealed trait Expr[-A, +B]
  object Expr {
    case object Unit extends Expr[Any, Unit]
    final case class Source[A <: ColumnSet, B](table: Table[A], column: Column[B]) extends Expr[A, B]
    final case class Binary[A, B](left: Expr[A, B], right: Expr[A, B], op: BinaryOp[B]) extends Expr[A, B]
    final case class Literal[B](value: B) extends Expr[Any, B]
  }

  /**
   * Models a function `A => Boolean` that decides whether to retain elements
   * in a source structure `A`.
   * 
   * {{{
   * WHERE (person(age) * 2) >= lit(32) && person(name).isNotNull
   * }}}
   */
  sealed trait Where[-A] { self =>
    def && [A1 <: A](that: Where[A1]): Where[A1] = Where.And(self, that)

    def || [A1 <: A](that: Where[A1]): Where[A1] = Where.Or(self, that)
  }
  object Where {
    final case class And[A](left: Where[A], right: Where[A]) extends Where[A]
    final case class Or[A](left: Where[A], right: Where[A]) extends Where[A]
    final case class Relation[A, B](expr: Expr[A, B], predicate: Predicate[B]) extends Where[A]
  }

  sealed trait Predicate[-A]
  object Predicate {
    final case class Equals[A](right: A) extends Predicate[A]
    case object IsNull extends Predicate[Any]
    case object IsNotNull extends Predicate[Any]
  }
}

/*
select(age * 2 ~ name ~ username)
  .from(person)
  .where(age === lit(42))
*/
