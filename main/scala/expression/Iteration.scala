package expression
import context.Environment
import value.{Boole, Notification, Value}
//use while loop
//while(condition) body
//while condition not false, execute body until condition is false
case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    while (condition.execute(env) == Boole.TRUE) body.execute(env)
    Notification.DONE
  }
}
