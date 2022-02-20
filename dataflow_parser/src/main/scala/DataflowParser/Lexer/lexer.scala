package DataflowParser.Lexer

import scala.util.parsing.combinator.{RegexParsers, Parsers}
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
  def aliasOpToken: Parser[DataflowToken]      = """as""" ^^ (_ => AliasOpToken)
  def assignEqToken: Parser[DataflowToken]     = """[\=]|[\:]""".r ^^ (_ => AssignEqToken)

  def numberToken: Parser[DataflowToken]       = """[0-9]+(\.){0,1}[0-9]*""".r ^^ (rep => NumberToken(rep.toFloat))
  def literalToken: Parser[DataflowToken]      = """\'.*\'""".r ^^ (rep => LiteralToken(rep))
  def identifierToken: Parser[DataflowToken]   = """(\{{0,1}[a-zA-Z]+[a-zA-Z0-9\.\_]*\}{0,1})|(\$\$)""".r ^^ (id => IdentifierToken(id))

  def operationEquals: Parser[DataflowToken]   = """==""" ^^ (_ => OperationEquals)
  def operationLEq: Parser[DataflowToken]      = """\<\=""".r ^^ (_ => OperationLEq)
  def operationGEq: Parser[DataflowToken]      = """\>\=""".r ^^ (_ => OperationGEq)
  def operationLess: Parser[DataflowToken]     = """\<""".r ^^ (_ => OperationLess)
  def operationGreat: Parser[DataflowToken]    = """\>""".r ^^ (_ => OperationGreat)
  def operationPlus: Parser[DataflowToken]     = """\+""".r ^^ (_ => OperationPlus)
  def operationSubtract: Parser[DataflowToken] = """\-""".r ^^ (_ => OperationSubtract)
  def operationDiv: Parser[DataflowToken]      = """\/""".r ^^ (_ => OperationDiv)
  def operationProd: Parser[DataflowToken]     = """\*""".r ^^ (_ => OperationProd)
  def operationMod: Parser[DataflowToken]      = """\%""".r ^^ (_ => OperationMod)
  def operationAnd: Parser[DataflowToken]      = """\&\&""".r ^^(_ => OperationAnd)
  def operationOr: Parser[DataflowToken]       = """||""" ^^(_ => OperationOr)
   
  private def tokens: Parser[List[DataflowToken]] = // should be sorted by length to avoid early recognition
    phrase(
      rep1(identifierToken |
        literalToken       |
        numberToken        |
        leftParenToken     |
        rightParenToken    |
        leftBraceToken     |
        rightBraceToken    |
        separatorToken     |
        operationEquals    |
        assignOpToken      |
        assignEqToken      |
        aliasOpToken       |
        operationPlus      |
        operationSubtract  |
        operationAnd       |
        operationDiv       |
        operationProd      |
        operationMod       |
        operationOr        
      )
    )

  def tokenize(script: String): Either[String, List[DataflowToken]] =
    parse(tokens, script) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)
    }
}


class ExpressionTokenReader(tokens: List[DataflowToken]) extends Reader[DataflowToken] {
  def first: DataflowToken = tokens.head
  def atEnd: Boolean = tokens.isEmpty
  def pos: Position = NoPosition
  def rest: Reader[DataflowToken] = new ExpressionTokenReader(tokens.tail)
}



