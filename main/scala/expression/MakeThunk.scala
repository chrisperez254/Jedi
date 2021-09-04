package expression
import context.Environment
import value.{Thunk, Value}

//similar to lambda without parameters
case class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = new Thunk(body, env)
}
