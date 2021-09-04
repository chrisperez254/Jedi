package value

class Notification(val msg: String) extends Value{
  override def toString: String = msg;
}

object Notification {
  def apply(m: String) = new Notification(m)
  val OK = Notification("ok")
  val DONE = Notification("done")
  val UNSPECIFIED = Notification("unspecified")

}
