package value

import context.TypeException
import expression.Literal

case class Boole(val value: Boolean) extends Literal{
  //if this and other are both true return true
  //otherwise return false
  def &&(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  //if this and other are both false return false
  //otherwise return true
  def ||(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }


  def unary_!(): Boole = Boole(!this.value)

  override def equals(other: Any): Boolean =
    other match {
      case x: Boole => x.isInstanceOf[Boole] && x.value == this.value
      case _ => false
    }

  override def toString: String = this.value.toString
  override def hashCode = this.toString.hashCode
}

object Boole {
  def apply(value: Boolean) = new Boole(value)
  def FALSE = Boole(false)
  def TRUE = Boole(true)
}