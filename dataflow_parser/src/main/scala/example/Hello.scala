package example

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.matching.Regex




sealed trait DataflowToken
case class IdentifierToken(s: String) extends DataflowToken
case object LeftParenToken extends DataflowToken
case object RightParenToken extends DataflowToken
case object SeparatorToken extends DataflowToken
case object LeftBraceToken extends DataflowToken
case object RightBraceToken extends DataflowToken
case class AssignOpToken(rep: String) extends DataflowToken
case object AssignEqToken extends DataflowToken
case object AliasOpToken extends DataflowToken
case class LiteralToken(rep: String) extends DataflowToken
case class OperationToken(rep: String) extends DataflowToken
case class NumberToken(rep: String) extends DataflowToken




object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def leftParenToken: Parser[DataflowToken] = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken] = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken] = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken] = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken] = "," ^^ (_ => SeparatorToken)

  def assignOpToken: Parser[DataflowToken] = """(~>){1}""".r ^^ (rep => AssignOpToken(rep))
  def aliasOpToken: Parser[DataflowToken] = "as" ^^ (_ => AliasOpToken)
  def assignEqToken: Parser[DataflowToken] = """[\=]""".r ^^ (_ => AssignEqToken)

  def numberToken: Parser[DataflowToken] = """[0-9]+(\.){0,1}[0-9]*""".r ^^ (rep => NumberToken(rep))
  def literalToken: Parser[DataflowToken] = """\'[a-zA-Z0-9\\-]+\'""".r ^^ (rep => LiteralToken(rep))
  def identifierToken: Parser[DataflowToken] = """[a-zA-Z]+[a-zA-Z0-9]*""".r ^^ (id => IdentifierToken(id))

  def operationToken: Parser[DataflowToken] = """(\-|\+|\=\=|&&|\|\||<|>|<\=|>\=|\=\=\=|\=\!\=)""".r ^^ (rep => OperationToken(rep))
   
  private def tokens: Parser[List[DataflowToken]] =
    phrase(
      rep1(identifierToken |
        leftParenToken |
        rightParenToken|
        leftBraceToken |
        rightBraceToken|
        separatorToken |
        assignOpToken  |
        assignEqToken  |
        aliasOpToken   |
        literalToken   |
        operationToken |
        numberToken
      )
    )

  def tokenize(script: String): ParseResult[List[DataflowToken]] = parse(tokens, script)
}




sealed trait ExpressionAST
case class Id(value: String) extends ExpressionAST
case class Number(value: String) extends ExpressionAST
case class Assign(id: String, value: String) extends ExpressionAST
case class FuncCall(func: String, args: ExpressionAST) extends ExpressionAST



class ExpressionTokenReader(tokens: List[DataflowToken]) extends Reader[DataflowToken] {
  def first: DataflowToken = tokens.head
  def atEnd: Boolean = tokens.isEmpty
  def pos: Position = NoPosition
  def rest: Reader[DataflowToken] = new ExpressionTokenReader(tokens.tail)
}



object ExpressionParser extends Parsers {
  override type Elem = DataflowToken

  private def id = accept("Id", { case IdentifierToken(v) => Id(v)})
  private def number = accept("Number", { case NumberToken(n) => Number(n)})
  private def terminal: Parser[ExpressionAST] = id | number

  private def program: Parser[ExpressionAST] = phrase(expr)

  private def asign: Parser[ExpressionAST] = 
    (id ~ rep1(AssignEqToken) ~ number) ^^ {case Id(i) ~ op ~ Number(n) => Assign(i, n)}

  private def funcCall: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ expr ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ ex ~ _ => FuncCall(i, ex)
    }

  private def expr: Parser[ExpressionAST] = asign | funcCall
  

  def parseFromTokens(input: List[DataflowToken]) = {
    val reader = new ExpressionTokenReader(input)
    expr(reader)
  }
}




object Hello extends Greeting with App {
  //println(ScriptLexer.tokenize(test2))
  val tokens = ScriptLexer.tokenize(test3)
  println(tokens)
  tokens.map(
    tks => ExpressionParser.parseFromTokens(tks)
  )
  .map(println)
}

trait Greeting {
  lazy val test1: String =
    """
source(output(
        'movieId' as string,
        title as string,
        genres as string,
        aux = 23
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

  lazy val test3: String =
    """
        source(input)
"""
}
