package value

//alu cons takes 2 inputs and returns a new input as a pair
//alu car takes a pair as input and returns the first element
//alu cdr takes a pair as input and returns the second element
//alu function for nil
//alu function list, recursively makes everything part of the list and cons the first element
//in his problems section, write jedi functions, not scala functions
case class Pair(first: Value, second: Value) extends Value {
  override def toString: String = "(" + first.toString + ", " + second.toString + ")"
}
