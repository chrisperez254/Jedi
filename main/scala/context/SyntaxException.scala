package context

//import scala.reflect.macros.Parsers
import scala.util.parsing.combinator.Parsers
import expression.Identifier

class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error") {

}
