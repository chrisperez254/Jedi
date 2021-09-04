package value

//a singleton value that prints as Nil and represents the empty list.
object empty extends Value {
  override def toString: String = "Nil"
}
