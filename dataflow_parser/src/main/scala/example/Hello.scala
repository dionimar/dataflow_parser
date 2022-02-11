package example

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}
import scala.util.matching.Regex




trait DataflowToken
case class IdentifierToken(s: String) extends DataflowToken
case object LeftParenToken extends DataflowToken
case object RightParenToken extends DataflowToken
case object SeparatorToken extends DataflowToken
case object LeftBraceToken extends DataflowToken
case object RightBraceToken extends DataflowToken
case object AssignOpToken extends DataflowToken
case object AssignEqToken extends DataflowToken
case object AliasOpToken extends DataflowToken
case class LiteralToken(rep: String) extends DataflowToken
case class OperationToken(rep: String) extends DataflowToken
case class NumberToken(value: Float) extends DataflowToken

sealed trait OperatorToken extends DataflowToken
case object OperationEquals extends OperatorToken
case object OperationPlus extends OperatorToken





object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def leftParenToken: Parser[DataflowToken] = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken] = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken] = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken] = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken] = "," ^^ (_ => SeparatorToken)

  def assignOpToken: Parser[DataflowToken] = """(~>){1}""".r ^^ (_ => AssignOpToken)
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
case class Transformation(depends: String, definition: ExpressionAST, output: String) extends ExpressionAST



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

  //private def operationSeq: Parser[ExpressionAST] = (OperationPlus ~ expr) ^^ {case _ ~ e => e}

  private def operation: Parser[ExpressionAST] = {
    val endOp = (OperationPlus ~ (terminal | funcCall)) ^^ {case _ ~ t => t}
    val opAdd = (funcCall | terminal) ~ OperationPlus ~ (endOp | expr) ^^ {
      case i ~ _ ~ ex => Operation("Add", i, ex)
    }
    opAdd
  }

  private def arg: Parser[ExpressionAST] = (SeparatorToken ~ expr) ^^ {case _ ~ e => e}

  private def funcCall: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ expr ~ (arg.*) ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ ex1 ~ rest ~ _ => FuncCall(i, ex1 :: rest)
    }

  private def expr: Parser[ExpressionAST] =
    operation | funcCall | asign | terminal

  private def step: Parser[ExpressionAST] =
    (terminal.* ~ funcCall ~ AssignOpToken ~ terminal) ^^ {
      case dependants ~ definition ~ _ ~ name => Transformation(dependants.toString, definition, name.toString)
    }
  
  private def program: Parser[ExpressionAST] = phrase(step)

  def parseFromTokens(input: List[DataflowToken]) = {
    val reader = new ExpressionTokenReader(input)
    expr(reader)
  }
}




object Hello extends Greeting with App {

  def printAST(indent: Integer, ast: ExpressionAST): Unit = ast match {
    case Id(name) => {print(List.fill(indent)("\t").mkString); println(name)}
    case Number(n) => {print(List.fill(indent)("\t").mkString); println(n)}
    case Assign(id, expr) => {print(List.fill(indent)("\t").mkString); println(id); printAST(indent + 1, expr)}
    case Operation(op, ex1, ex2) => {print(List.fill(indent)("\t").mkString); println(op); printAST(indent + 1, ex1);printAST(indent + 1, ex2)}
    case FuncCall(id, args) => {print(List.fill(indent)("\t").mkString); println("call " + id); args.map(x => printAST(indent + 1, x))}
    case Transformation(deps, definition, output) => {
      print(List.fill(indent)("\t").mkString);
      println(deps);
      print(List.fill(indent)("\t").mkString);
      printAST(indent+1, definition);
      print(List.fill(indent)("\t").mkString);
      println(output);
    }
  }



  val test = test1

  println(test)
  val tokens = ScriptLexer.tokenize(test)
  println(tokens)
  tokens.map(
    tks => ExpressionParser.parseFromTokens(tks)
  )
  .map(_.map(x => printAST(0,x)))
}

trait Greeting {
  lazy val test1: String =
    """
aux source(output(
        1,
        aux + 23,
        input(Z)
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
X+f(0+1)+Z+123
"""
}
