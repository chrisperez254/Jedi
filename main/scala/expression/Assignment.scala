package expression
import context.{Environment, TypeException}
import value.{Boole, Notification, Value, Variable}

//x = 2 + 3
//x vbl
//2 + 3 update
case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    //check if vbl is a variable, if not throw TypeException
    def v = vbl.execute(env)
    def x = update.execute(env)
    if (!v.isInstanceOf[Variable]) throw new TypeException("Needs to be a variable")
    else vbl.execute(env).asInstanceOf[Variable].content = x
    //if it is update and load that into the varaible
    Notification.OK
  }
}
