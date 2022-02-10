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
case object OperationEquals extends DataflowToken
case object OperationPlus extends DataflowToken
case class NumberToken(value: Float) extends DataflowToken




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

  def numberToken: Parser[DataflowToken] = """\-{0,1}[0-9]+(\.){0,1}[0-9]*""".r ^^ (rep => NumberToken(rep.toFloat))
  def literalToken: Parser[DataflowToken] = """\'[a-zA-Z0-9\\-]+\'""".r ^^ (rep => LiteralToken(rep))
  def identifierToken: Parser[DataflowToken] = """[a-zA-Z]+[a-zA-Z0-9]*""".r ^^ (id => IdentifierToken(id))

  //def operationToken: Parser[DataflowToken] = """(\-|\+|\=\=|&&|\|\||<|>|<\=|>\=|\=\=\=|\=\!\=)""".r ^^ (rep => OperationToken(rep))

  def operationEquals: Parser[DataflowToken] = """\=\=""".r ^^ (_ => OperationEquals)
  def operationPlus: Parser[DataflowToken] = """\+""".r ^^ (_ => OperationPlus)
   
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
        //operationToken |
        operationEquals |
        operationPlus  |
        numberToken
      )
    )

  def tokenize(script: String): ParseResult[List[DataflowToken]] = parse(tokens, script)
}




sealed trait ExpressionAST
case class Id(value: String) extends ExpressionAST
case class Number(value: Float) extends ExpressionAST
case class Assign(id: String, value: ExpressionAST) extends ExpressionAST
case class Operation(op: String, arg1: ExpressionAST, arg2: ExpressionAST) extends ExpressionAST
case class FuncCall(func: String, args: List[ExpressionAST]) extends ExpressionAST



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

  private def asign: Parser[ExpressionAST] = 
    (id ~ AssignEqToken ~ (terminal | expr)) ^^ {case Id(i) ~ op ~ value => Assign(i, value)}

  private def operation: Parser[ExpressionAST] = {
    val opEq = (terminal | expr) ~ OperationEquals ~ (expr | terminal) ^^ {
      case i ~ _ ~ ii => Operation("Equals", i, ii)
    }
    val opAdd = (terminal | expr) ~ OperationPlus ~ (expr | terminal) ^^ {
      case i ~ _ ~ ii => Operation("Add", i, ii)
    }
    opEq | opAdd
  }

  private def arg: Parser[ExpressionAST] = (SeparatorToken ~ expr) ^^ {case _ ~ e => e}

  private def funcCall: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ expr ~ (arg.*) ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ ex1 ~ rest ~ _ => FuncCall(i, ex1 :: rest)
    }

  private def expr: Parser[ExpressionAST] = operation | funcCall | terminal | asign
  
  private def program: Parser[ExpressionAST] = phrase(expr)

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
source(asdf, basdf,-23)
"""
}
