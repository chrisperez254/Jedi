package context

import value._
import expression._

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "same" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
    case "write" => write(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "cons" => cons(args)
    case "nil" => nil()
    case "list" => list(args)
    // variables
    case "dereference" => dereference(args)
      //takes variable as input
      //makes sure args(0) is a variable and return contents
      //it just dereferences them
    case "var" => makeVar(args)
      //similar to pair just one instead of two


    // store ops
    /*
    case "store" => store(args)
    case "put" => put(args)
    case "rem" => rem(args)
    case "contains" => contains(args)
    case "map" => map(args)
    case "filter" => filter(args)
    case "get" => get(args)
    case "addLast" => addLast(args)
    case "size" => size(args)
    */
    case _ => throw new UndefinedException(opcode)
  }

  //add in those functions
  //change addable to numeric
  private def add(args: List[Value]): Value = {

    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result * unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to * must be Numeric")
    }
  }

  private def sub(args: List[Value]): Numeric = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to - must be Numeric")
    }
  }

  private def div(args: List[Value]): Numeric = {

    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if(unseen == Nil) result
      else helper(result / unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to / must be Numeric")
    }
  }


  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def same(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    //if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to == must be orderable")
    Boole(args(0) == args(1))
  }

  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    //if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to != must be orderable")
    Boole(args(0) != args(1))
  }

  private def not(args: List[Value]): Value = {
    if(args.size != 1) throw new TypeException("1 input required by !")
    if(!args(0).isInstanceOf[Boole]) throw new TypeException("Input to ! must be Boole")
    val check = args(0).asInstanceOf[Boole].value
    Boole(!check)
  }

  private def write(args: List[Value]):Value = {
    for (ele <- args) {
      println(ele)
    }
    Notification.DONE
  }

  //alu cons takes 2 inputs and returns a new input as a pair
  //alu function list, recursively makes everything part of the list and cons the first element
  //list: recusrively make a list of the second element of the pair
  //make a new pair where the left element is the first and then the recusive call to the pair(the rest) is the second
  private def car(args: List[Value]): Value = args(0).asInstanceOf[Pair].first
  private def cdr(args: List[Value]): Value = args(0).asInstanceOf[Pair].second
  private def cons(args: List[Value]): Value = {
    val v1: Value = args(0)
    val v2: Value = args(1)
    Pair(v1, v2)
  }
  private def nil(): Value = empty
  private def list(args: List[Value]): Value = if (args == Nil) nil() else Pair(args.head, list(args.tail))

  //takes variable as input
  //makes sure args(0) is a variable and return contents
  //it just dereferences them
  private def dereference(args: List[Value]): Value = args(0).asInstanceOf[Variable].content
  //similar to pair just one instead of two
  private def makeVar(args: List[Value]): Value = Variable(args(0))
}
