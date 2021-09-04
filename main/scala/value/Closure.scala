package value

import context.{Environment, TypeException}
import expression.{Expression, Identifier}

class Closure(defEnv: Environment, body: Expression, parameters: List[Identifier]) extends Value {
  def apply(args: List[Value]):Value = {
    val tempEnv = new Environment(defEnv)
    if (parameters.length != args.length) throw new TypeException(" #args != #parameters")
    else {
      for (i<-0 until parameters.length) {
        tempEnv.bulkPut(parameters, args)
      }
    }
    body.execute(tempEnv)
  }
}
