package expression
import context.{Environment, TypeException}
import value.{Boole, Value}

case class Conjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var b = true
    for (i <- operands if b) {
      val x = i.execute(env)
      if (!x.isInstanceOf[Boole])
        throw new TypeException("Argument has to be a Boole")
      b = x.asInstanceOf[Boole].value
    }
    Boole(b)
  }
}
