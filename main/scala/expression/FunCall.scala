package expression
import context.{Environment, alu, flags}
import value.{Closure, Thunk, Value}

case class FunCall(operator: Identifier,  operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    var args:List[Value] = Nil
    //var args = operands.map(_.execute(env)) //this will change put in else clause
    //if operator is a closure then call closure.apply()
    if (env.contains(operator)) {
      if (flags.paramPassing == flags.BY_NAME) {
        args = operands.map(MakeThunk(_).execute(env))
      } else {
        args = operands.map(_.execute(env))
      }

      operator.execute(env) match{
        case t: Thunk=> t.apply()
        case c: Closure => c.apply(args)
      }
    } else {
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }
  }
}
