package zio.sql

trait Sql {
  type ColumnName = String
  type TableName = String
  sealed case class ColumnSchema[A](value: A)

  sealed trait ColumnSet {
    type ColumnsRepr[T]

    def :*: [A](head: Column[A]): ColumnSet

    def columnsUntyped: List[Column[_]]

    protected def mkColumns[T]: ColumnsRepr[T]
  }
  object ColumnSet {
    type Empty = Empty.type
    type :*: [A, B <: ColumnSet] = Cons[A, B]

    case object Empty extends ColumnSet {
      type ColumnsRepr[_] = Unit

      override def :*: [A](head: Column[A]): A :*: Empty = Cons(head, Empty)

      override def columnsUntyped: List[Column[_]] = Nil

      override protected def mkColumns[T]: ColumnsRepr[T] = ()
    }
    sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>     
      type ColumnsRepr[T] = (Expr[T, A], tail.ColumnsRepr[T])

      override def :*: [C](head: Column[C]): C :*: A :*: B = Cons(head, self)

      override def columnsUntyped: List[Column[_]] = head :: tail.columnsUntyped

      def table(name0: TableName): Table.Source[ColumnsRepr, A :*: B] = 
        new Table.Source[ColumnsRepr, A :*: B] {
          val name: TableName = name0
          val columnSchema: ColumnSchema[A :*: B] = ColumnSchema(self)
          val columns: ColumnsRepr[TableType] = mkColumns[TableType]
          val columnsUntyped: List[Column[_]] = self.columnsUntyped
        }

      override protected def mkColumns[T]: ColumnsRepr[T] = 
        (Expr.Source(head), tail.mkColumns)
    }
    object :*: {
      def unapply[A, B](tuple: (A, B)): Some[(A, B)] = Some(tuple)
    }
    import Column._ 

    val columnSet = int("age") :*: string("name") :*: Empty

    val table = columnSet.table("person")

    val age :*: name :*: _ = table.columns
  }

  sealed trait TypeTag[A]
  object TypeTag {
    implicit case object TInt extends TypeTag[Int]
    implicit case object TLong extends TypeTag[Long]
    implicit case object TBoolean extends TypeTag[Boolean]
    implicit case object TString extends TypeTag[String]
  }

  sealed case class FunctionName(name: String)

  sealed case class Column[A: TypeTag](name: String) {
    def typeTag: TypeTag[A] = implicitly[TypeTag[A]]
  }
  object Column {
    def int(name: String): Column[Int] = Column[Int](name)
    def long(name: String): Column[Long] = Column[Long](name)
    def string(name: String): Column[String] = Column[String](name)
  }

  sealed trait Table {
    type TableType
  }
  object Table {
    type Aux[A] = Table { type TableType = A }

    sealed trait Source[F[_], A] extends Table {
      val name: TableName
      val columnSchema: ColumnSchema[A]
      val columns: F[TableType]
      val columnsUntyped: List[Column[_]]
    } 
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
    sealed case class Select[A, B <: SelectionSet[A]](
      selection: Selection[A, B], table: Table.Aux[A], where: Expr[A, Boolean]) extends Read[B]
    
    sealed case class Union[B](left: Read[B], right: Read[B], distinct: Boolean) extends Read[B]

    sealed case class Literal[B: TypeTag](values: Iterable[B]) extends Read[B]
  }

  /**
   * A columnar selection of `B` from a source `A`, modeled as `A => B`.
   */
  sealed case class Selection[-A, +B <: SelectionSet[A]](value: B)
  object Selection {
    import SelectionSet.{ Empty, Cons }
    import ColumnSelection._

    val empty: Selection[Any, Empty] = Selection(Empty)

    def constantOption[A: TypeTag](value: A, option: Option[ColumnName]): Selection[Any, Cons[Any, A, Empty]] = 
      Selection(Cons(Constant(value, option), Empty))

    def constant[A: TypeTag](value: A): Selection[Any, Cons[Any, A, Empty]] = constantOption(value, None)

    def constantAs[A: TypeTag](value: A, name: ColumnName): Selection[Any, Cons[Any, A, Empty]] = constantOption(value, Some(name))

    def computedOption[A, B](expr: Expr[A, B], name: Option[ColumnName]): Selection[A, Cons[A, B, Empty]] =
      Selection(Cons(Computed(expr, name), Empty))

    def computed[A, B](expr: Expr[A, B]): Selection[A, Cons[A, B, Empty]] =
      computedOption(expr, None)

    def computedAs[A, B](expr: Expr[A, B], name: ColumnName): Selection[A, Cons[A, B, Empty]] =
      computedOption(expr, Some(name))
  }
  
  sealed trait ColumnSelection[-A, +B]
  object ColumnSelection {
    sealed case class Constant[A: TypeTag](value: A, name: Option[ColumnName]) extends ColumnSelection[Any, A]
    sealed case class Computed[A, B](expr: Expr[A, B], name: Option[ColumnName]) extends ColumnSelection[A, B]
  }

  sealed trait SelectionSet[-Source] {
    type SelectionsRepr[T]

    def :*: [Source1 <: Source, A](head: ColumnSelection[Source1, A]): SelectionSet[Source1]

    def selectionsUntyped: List[ColumnSelection[Source, _]]

    //protected def mkSelections[T]: SelectionsRepr[T]
  }
  object SelectionSet {
    type Empty = Empty.type

    case object Empty extends SelectionSet[Any] {
      type SelectionsRepr[_] = Unit

      override def :*: [Source1 <: Any, A](head: ColumnSelection[Source1, A]) = Cons(head, Empty)

      override def selectionsUntyped: List[ColumnSelection[Any, _]] = Nil

      //override protected def mkSelections[T]: SelectionsRepr[T] = ()
    }
    sealed case class Cons[Source, A, B <: SelectionSet[Source]](head: ColumnSelection[Source, A], tail: B) extends SelectionSet[Source] { self =>     
      type SelectionsRepr[T] = (Expr[T, A], tail.SelectionsRepr[T])

      override def :*: [Source1 <: Source, C](head: ColumnSelection[Source1, C]) = Cons(head, self)

      override def selectionsUntyped: List[ColumnSelection[Source, _]] = head :: tail.selectionsUntyped

      // def table(name0: TableName): Table.Source[SelectionsRepr, Cons[Source, A, B]] = 
      //   new Table.Source[SelectionsRepr, Cons[Source, A, B]] {
      //     val name: TableName = name0
      //     val selectionSchema: SelectionSchema[A :*: B] = SelectionSchema(self)
      //     val selections: SelectionsRepr[TableType] = mkSelections[TableType]
      //     val selectionsUntyped: List[ColumnSelection[_]] = self.selectionsUntyped
      //   }

      // override protected def mkSelections[T]: SelectionsRepr[T] = 
      //   (Expr.Source(head), tail.mkSelections)
    }
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
    sealed case class Source[A, B] private [Sql] (column: Column[B]) extends Expr[A, B]
    sealed case class Binary[A, B](left: Expr[A, B], right: Expr[A, B], op: BinaryOp[B]) extends Expr[A, B]
    sealed case class In[A, B](value: Expr[A, B], set: Read[B]) extends Expr[A, Boolean]
    sealed case class Literal[B: TypeTag](value: B) extends Expr[Any, B]
    sealed case class FunctionCall[A, B, C](value: Expr[A, B], function: FunctionDef[B, C])
      extends Expr[A, C]
    // a IN ("foo", "bar")
  }

  sealed case class FunctionDef[-A, +B](name: FunctionName)
}

/*
 def query(limit: Int) = 
    select(age * 2 ~ name ~ username)
      .from(person)
      .where(age === lit(42))
      .limit(limit)

val query = 
  Param.int { limit =>
    select(age * 2 ~ name ~ username)
      .from(person)
      .where(age === lit(42))
      .limit(limit)
  }

  query(200)
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