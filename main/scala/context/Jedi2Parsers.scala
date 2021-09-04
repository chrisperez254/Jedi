package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]]  = "(" ~ opt(identifier ~ rep("," ~> identifier)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => List()
    case "(" ~ Some(param ~ more) ~ ")" => param :: more
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda]  = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ params ~ expression => Lambda(params, expression)
  }

  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a MakeThunk
  def freeze: Parser[MakeThunk]  = "freeze" ~ "(" ~ expression ~ ")" ^^ {
    case "freeze" ~ "(" ~ expression ~ ")" => MakeThunk(expression)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block]  = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ a ~ more ~ "}" => Block(a :: more)
  }

  //watch out for freeze and thunk, their order here matters
  // override of term parser
  override def term: Parser[Expression]  = lambda | freeze | funCall | block | literal | "("~>expression<~")"
}

