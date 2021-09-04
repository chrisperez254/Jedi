package value

//should be done in same way as Addable
trait Numeric extends Addable {
  def *(other: Value): Numeric
  def -(other: Value): Numeric
  def /(other: Value): Numeric
  def unary_-(): Numeric
}
