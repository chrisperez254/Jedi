package value

import context.{IllegalValueException, TypeException}
import expression.Literal

//chars and booles follow the same basic pattern
case class Exact(val value: Int) extends Numeric with Ordered[Value] {

  def +(other: Value): Addable =
    other match {
        //got to unwrap and rewrap our values
      case x: Exact => Exact(this.value + x.value)
      case x: Inexact => Inexact(this.value.toDouble + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  override def *(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value * x.value)
      case x: Inexact => Inexact(this.value.toDouble * x.value)
      case _ => throw new TypeException("Numeric operand required")
  }

  override def -(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value - x.value)
      case x: Inexact => Inexact(this.value.toDouble - x.value)
      case _ => throw new TypeException("Numeric operand required")
  }

  override def /(other: Value): Numeric = {
    other match {
      case x: Exact => if (x.value == 0) throw new IllegalValueException("Divide by 0!") else Exact(this.value / x.value)
      case x: Inexact => if (x.value == 0.0) throw new IllegalValueException("Divide by 0!") else Inexact(this.value.toDouble / x.value)
      case _ => throw new TypeException("Numeric operand required")
  }
  }

  override def unary_-(): Numeric = Exact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value)
      case x: Inexact => this.value.toDouble.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }


  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }

  override def toString: String = this.value.toString

  override def hashCode = this.toString.hashCode

}
