package zio.sql

private[sql] class Renderer(val builder: StringBuilder) extends AnyVal {
  //not using vararg to avoid allocating `Seq`s
  def apply(s1: Any): Unit = {
    val _ = builder.append(s1)
  }
  def apply(s1: Any, s2: Any): Unit = {
    val _ = builder.append(s1).append(s2)
  }
  def apply(s1: Any, s2: Any, s3: Any): Unit = {
    val _ = builder.append(s1).append(s2).append(s3)
  }
  def apply(s1: Any, s2: Any, s3: Any, s4: Any): Unit = {
    val _ = builder.append(s1).append(s2).append(s3).append(s4)
  }

  override def toString: String = builder.toString()
}

private[sql] object Renderer {
  def apply(): Renderer = new Renderer(new StringBuilder)
}
