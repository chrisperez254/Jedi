package expression
import context.Environment
import value.{Chars, Notification, Value}

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var expValue = exp.execute(env)
    env.put(id, expValue)
    Notification.OK
  }

}
