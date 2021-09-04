package expression

import context.Environment
import value.{Thunk, Value}

case class Identifier(val name: String) extends Expression {
  override def toString = name
  override def execute(env: Environment)  = env(this) match {
    case v:Value => if(v.isInstanceOf[Thunk]) v.asInstanceOf[Thunk].apply() else v
  }
}
