package example

import scala.util.parsing.combinator._
import scala.util.matching.Regex



sealed trait DataflowToken
case class IdentifierToken(s: String) extends DataflowToken
case object LeftParenToken extends DataflowToken
case object RightParenToken extends DataflowToken
case object SeparatorToken extends DataflowToken
case object LeftBraceToken extends DataflowToken
case object RightBraceToken extends DataflowToken
case class AssignOpToken(rep: String) extends DataflowToken
case object AliasOpToken extends DataflowToken
case class LiteralToken(rep: String) extends DataflowToken
case class OperationToken(rep: String) extends DataflowToken




object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def identifierToken: Parser[DataflowToken] = """[a-zA-Z0-9]+""".r ^^ (id => IdentifierToken(id))
  def leftParenToken: Parser[DataflowToken] = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken] = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken] = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken] = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken] = "," ^^ (_ => SeparatorToken)
  def assignOpToken: Parser[DataflowToken] = """([=:])|(~>)""".r ^^ (rep => AssignOpToken(rep))
  def aliasOpToken: Parser[DataflowToken] = "as" ^^ (_ => AliasOpToken)
  def literalToken: Parser[DataflowToken] = """\'[a-zA-Z0-9\\-]+\'""".r ^^ (rep => LiteralToken(rep))
  def operationToken: Parser[DataflowToken] = """(\-|\+|\=\=)""".r ^^ (rep => OperationToken(rep))
   
  private def tokens: Parser[List[DataflowToken]] =
    phrase(
      rep1(identifierToken |
        leftParenToken |
        rightParenToken|
        leftBraceToken |
        rightBraceToken|
        separatorToken |
        assignOpToken  |
        aliasOpToken   |
        literalToken   |
        operationToken
      )
    )

  def tokenize(script: String): ParseResult[List[DataflowToken]] = parse(tokens, script)
}





sealed trait TransformationAST
case class AbstractTerminal(rep: String) extends TransformationAST
case class FunctionCall(funcName: IdentifierToken, args: TransformationAST) extends TransformationAST

object TransformationParser extends Parsers {
  override type Elem = DataflowToken
  private def terminal: Parser[TransformationAST] = AbstractTerminal()
  //private def formula: Parser[TransformationAST] = phrase()
}



sealed trait DataflowScript
case class Dependants(value: IdentifierToken) extends DataflowScript
//case class Transformation()


object ScriptParser extends Parsers {

}







object Hello extends Greeting with App {
  println(ScriptLexer.tokenize(test2))
}

trait Greeting {
  lazy val test1: String =
    """
source(output(
        'movieId' as string,
        title as string,
        genres as string
    )) ~> source1
"""

  lazy val test2: String = 
  """
source1 keyGenerate(output(sk as long),
	startAt: 1L) ~> SurrogateKey1
SurrogateKey1 derive(dummy = 1) ~> DerivedColumn1
DerivedColumn1 window(over(dummy),
	asc(sk, true),
	prevAndCurr = lag(title,1)+'-'+last(title),
		nextAndCurr = lead(title,1)+'-'+last(title)) ~> leadAndLag
"""
}
