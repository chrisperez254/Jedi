package expression
import context.Environment
import value.{Closure, Value}

case class Lambda(parameters: List[Identifier], body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    //return Closure object
    new Closure(env, body, parameters)
  }
}
