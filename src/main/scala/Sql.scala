package zio.sql

trait Sql {
  type ColumnName = String
  type TableName
  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]

    def :*: [A](head: Column[A]): ColumnSet

    protected def mkColumns[T](table: Table[T]): ColumnsRepr[T]
  }
  object ColumnSet {
    type Empty = Empty.type
    type :*: [A, B <: ColumnSet] = Cons[A, B]

    case object Empty extends ColumnSet {
      type ColumnsRepr[_] = Unit

      override def :*: [A](head: Column[A]): A :*: Empty = Cons(head, Empty)

      override protected def mkColumns[T](table: Table[T]): ColumnsRepr[T] = ()
    }
    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>     
      type ColumnsRepr[T] = (Expr[T, A], tail.ColumnsRepr[T])

      override def :*: [C](head: Column[C]): C :*: A :*: B = Cons(head, self)

      def columns(table: Table[A :*: B]): ColumnsRepr[A :*: B] = 
        mkColumns(table)

      override protected def mkColumns[T](table: Table[T]): ColumnsRepr[T] = 
        (Expr.Source(table, head), tail.mkColumns(table))
    }
    object :*: {
      def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
    }
    import Column._ 

    val columnSet = int("age") :*: string("name") :*: Empty

    def table: Table[Int :*: String :*: Empty] = ???

    val age :*: name :*: _ = columnSet.columns(table)
  }

  sealed trait TypeTag[A]
  object TypeTag {
    case object TInt extends TypeTag[Int]
    case object TLong extends TypeTag[Long]
    case object TBoolean extends TypeTag[Boolean]
    case object TString extends TypeTag[String]
  }

  sealed trait SqlType[A] {
    def typeTag: TypeTag[A]
  }
  object SqlType {
    implicit val SqlTypeBoolean: SqlType[Boolean] = new SqlType[Boolean] {
      val typeTag = TypeTag.TBoolean
    }
    implicit val SqlTypeInt: SqlType[Int] = new SqlType[Int] {
      val typeTag = TypeTag.TInt 
    }
    implicit val SqlTypeLong: SqlType[Long] = new SqlType[Long] {
      val typeTag = TypeTag.TLong
    }
    implicit val SqlTypeString: SqlType[String] = new SqlType[String] {
      val typeTag = TypeTag.TString
    }
  }

  sealed case class FunctionName(name: String)

  sealed case class Column[A: SqlType](name: String) {
    def typeTag: TypeTag[A] = implicitly[SqlType[A]].typeTag
  }
  object Column {
    def int(name: String): Column[Int] = Column[Int](name)
    def long(name: String): Column[Long] = Column[Long](name)
    def string(name: String): Column[String] = Column[String](name)
  }

  sealed trait Table[A] { self =>
    def apply[B](f: Table[A] => Column[B]): Expr[A, B] = Expr.Source(self, f(self))
  }
  object Table {
    sealed case class Source[A](name: TableName, columnSchema: ColumnSchema[A]) extends Table[A]
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
    sealed case class Select[A, B](
      selection: Selection[A, B], table: Table[A], where: Expr[A, Boolean]) extends Read[B]
    
    sealed case class Union[B](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B]

    sealed case class Literal[B: SqlType](values: Iterable[B]) extends Read[B]
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed trait Selection[-A, +B]
  object Selection {
    sealed case class Identity[A]() extends Selection[A, A]
    sealed case class Constant[A: SqlType](value: A) extends Selection[Any, A]
    sealed case class Concat[A, L, R](left: Selection[A, L], right: Selection[A, R]) 
      extends Selection[A, (L, R)]
    sealed case class Computed[A, B](expr: Expr[A, B], name: Option[ColumnName]) extends 
      Selection[A, B]
  }

  sealed trait BinaryOp[A]
  object BinaryOp {
    case object AddInt extends BinaryOp[Int]
    case object SubInt extends BinaryOp[Int]
    case object MultInt extends BinaryOp[Int]
    case object DivInt extends BinaryOp[Int]
    case object ModInt extends BinaryOp[Int]
    case object AddLong extends BinaryOp[Long]
    case object SubLong extends BinaryOp[Long]
    case object MultLong extends BinaryOp[Long]
    case object DivLong extends BinaryOp[Long]
    case object ModLong extends BinaryOp[Long]
    case object AndBool extends BinaryOp[Boolean]
    case object OrBool extends BinaryOp[Boolean]
    case object StringConcat extends BinaryOp[String]
  }

  /**
   * Models a function `A => B`.
   * SELECT product.price + 10
   */
  sealed trait Expr[-A, +B]
  object Expr {
    sealed case class Source[A, B](table: Table[A], column: Column[B]) extends Expr[A, B]
    sealed case class Binary[A, B](left: Expr[A, B], right: Expr[A, B], op: BinaryOp[B]) extends Expr[A, B]
    sealed case class In[A, B](value: Expr[A, B], set: Read[B]) extends Expr[A, Boolean]
    sealed case class Literal[B: SqlType](value: B) extends Expr[Any, B]
    sealed case class FunctionCall[A, B, C](value: Expr[A, B], function: FunctionDef[B, C])
      extends Expr[A, C]
    // a IN ("foo", "bar")
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName)
}

/*
select(age * 2 ~ name ~ username)
  .from(person)
  .where(age === lit(42))
*/

/*
sealed trait Predicate[-A]
  // sealed trait ExistencePredicate extends Predicate[Any]
  // object Predicate {
  //   sealed case class Equal[A](right: A) extends Predicate[A]
  //   sealed case class NoEqual[A](right: A) extends Predicate[A]
  //   sealed case class GreaterThan[A](right: A) extends Predicate[A]
  //   sealed case class LessThan[A](right: A) extends Predicate[A]
  //   sealed case class GreaterThanOrEqual[A](right: A) extends Predicate[A]
  //   sealed case class LessThanOrEqual[A](right: A) extends Predicate[A]
  //   case object IsNull extends Predicate[Any]
  //   case object IsNotNull extends Predicate[Any]
  //   case object IsTrue extends Predicate[Boolean]
  //   case object IsNotTrue extends Predicate[Boolean]
  //   case object IsFalse extends Predicate[Boolean]
  //   case object IsNotFalse extends Predicate[Boolean]
  //   sealed case class Between[A](lower: A, upper: A) extends Predicate[A]
  //   sealed case class Like(right: String) extends Predicate[String]
    
  //   sealed case class In[A](right: Set[A]) extends Predicate[A] //could be applied to a subquery result
  //   sealed case class NotIn[A](right: Set[A]) extends Predicate[A] //could be applied to a subquery result
    
  //   sealed case class Exists(read: Read[_]) extends ExistencePredicate
  //   sealed case class NotExists(read: Read[_]) extends ExistencePredicate
    
  //   //TODO Any, All
  // }
*/