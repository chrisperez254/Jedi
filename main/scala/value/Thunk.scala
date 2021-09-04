package value

import context.Environment
import expression.Expression

//a thunk is simply a parameterless function
class Thunk(val body: Expression,val defEnv: Environment) extends Closure(defEnv, body, Nil ) {
  private var cache: Value = null

  def apply() = {
    if (cache == null) { cache = super.apply(Nil) }
    cache
  }
  //apply
  //checks the cache
  //if cache is null, execute the body relative to the defEnv
  //and store that value into the cache then return the cache
  //execute the body(super.apply(Nil))
  //if cache is empty load cache by executing his body and return cache
  //isn't empty doesn't equal null, return thing in cache
}
