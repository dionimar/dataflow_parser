package DataflowParser.Lexer

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.matching.Regex

import DataflowParser.Tokens._


object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def leftParenToken: Parser[DataflowToken]    = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken]   = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken]    = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken]   = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken]    = "," ^^ (_ => SeparatorToken)

  def assignOpToken: Parser[DataflowToken]     = """(~>){1}""".r ^^ (_ => AssignOpToken)
  def aliasOpToken: Parser[DataflowToken]      = """as""".r ^^ (_ => AliasOpToken)
  def assignEqToken: Parser[DataflowToken]     = """[\=]|[\:]""".r ^^ (_ => AssignEqToken)

  def numberToken: Parser[DataflowToken]       = """\-{0,1}[0-9]+(\.){0,1}[0-9]*""".r ^^ (rep => NumberToken(rep.toFloat))
  def literalToken: Parser[DataflowToken]      = """\'[a-zA-Z0-9\\-]+\'""".r ^^ (rep => LiteralToken(rep))
  def identifierToken: Parser[DataflowToken]   = """[a-zA-Z]+[a-zA-Z0-9]*""".r ^^ (id => IdentifierToken(id))

  def operationEquals: Parser[DataflowToken]   = """\=\=""".r ^^ (_ => OperationEquals)
  def operationPlus: Parser[DataflowToken]     = """\+""".r ^^ (_ => OperationPlus)
  def operationSubtract: Parser[DataflowToken] = """\-""".r ^^ (_ => OperationSubtract)
   
  private def tokens: Parser[List[DataflowToken]] = // should be sorted by length to avoid early recognition
    phrase(
      rep1(identifierToken |
        leftParenToken     |
        rightParenToken    |
        leftBraceToken     |
        rightBraceToken    |
        separatorToken     |
        operationEquals    |
        assignOpToken      |
        assignEqToken      |
        aliasOpToken       |
        literalToken       |
        operationPlus      |
        operationSubtract  |
        numberToken
      )
    )

  def tokenize(script: String): ParseResult[List[DataflowToken]] = parse(tokens, script)
}


class ExpressionTokenReader(tokens: List[DataflowToken]) extends Reader[DataflowToken] {
  def first: DataflowToken = tokens.head
  def atEnd: Boolean = tokens.isEmpty
  def pos: Position = NoPosition
  def rest: Reader[DataflowToken] = new ExpressionTokenReader(tokens.tail)
}
