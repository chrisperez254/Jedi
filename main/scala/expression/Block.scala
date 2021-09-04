package expression
import context.Environment
import value.Value

case class Block(operands:List[Expression]) extends SpecialForm with Expression {

  override def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    operands.map(_.execute(tempEnv)).last
  }

}