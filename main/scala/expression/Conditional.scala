package expression
import context.{Environment, TypeException}
import value.{Boole, Exact, Notification, Value}

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var b = new Value {}
    if (condition.execute(env) == Boole.TRUE) {
     b = consequent.execute(env)
    } else if (alternative != null) {
      b = alternative.execute(env)
    } else {
      b = Notification.UNSPECIFIED
    }
    b

  }
}
