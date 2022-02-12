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
case class NumberToken(value: Float) extends DataflowToken

sealed trait Operator extends DataflowToken
case object OperationPlus extends Operator
case object OperationEquals extends Operator
case object OperationSubtract extends Operator





object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def leftParenToken: Parser[DataflowToken] = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken] = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken] = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken] = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken] = "," ^^ (_ => SeparatorToken)

  def assignOpToken: Parser[DataflowToken] = """(~>){1}""".r ^^ (_ => AssignOpToken)
  def aliasOpToken: Parser[DataflowToken] = """as""".r ^^ (_ => AliasOpToken)
  def assignEqToken: Parser[DataflowToken] = """[\=]|[\:]""".r ^^ (_ => AssignEqToken)

  def numberToken: Parser[DataflowToken] = """\-{0,1}[0-9]+(\.){0,1}[0-9]*""".r ^^ (rep => NumberToken(rep.toFloat))
  def literalToken: Parser[DataflowToken] = """\'[a-zA-Z0-9\\-]+\'""".r ^^ (rep => LiteralToken(rep))
  def identifierToken: Parser[DataflowToken] = """[a-zA-Z]+[a-zA-Z0-9]*""".r ^^ (id => IdentifierToken(id))

  def operationEquals: Parser[DataflowToken] = """\=\=""".r ^^ (_ => OperationEquals)
  def operationPlus: Parser[DataflowToken] = """\+""".r ^^ (_ => OperationPlus)
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




sealed trait ExpressionAST
case class Id(value: String) extends ExpressionAST
case class Number(value: Float) extends ExpressionAST
case class Assign(id: String, value: ExpressionAST) extends ExpressionAST
case class Operation(op: String, arg1: ExpressionAST, arg2: ExpressionAST) extends ExpressionAST
case class FuncCall(func: String, args: List[ExpressionAST]) extends ExpressionAST
case class Transformation(depends: String, definition: ExpressionAST, output: String) extends ExpressionAST
case class Blocks(transformations: List[ExpressionAST]) extends ExpressionAST



class ExpressionTokenReader(tokens: List[DataflowToken]) extends Reader[DataflowToken] {
  def first: DataflowToken = tokens.head
  def atEnd: Boolean = tokens.isEmpty
  def pos: Position = NoPosition
  def rest: Reader[DataflowToken] = new ExpressionTokenReader(tokens.tail)
}



object ExpressionParser extends Parsers {
  override type Elem = DataflowToken

  private def id = accept("Id",
    {
      case IdentifierToken(v) => Id(v)
      case LiteralToken(v)    => Id(v)
    }
  )
  private def number = accept("Number", { case NumberToken(n) => Number(n)})
  private def terminal: Parser[ExpressionAST] = id | number


  private def asign: Parser[ExpressionAST] = 
    (id ~ AssignEqToken ~ (expr | terminal)) ^^ {case Id(i) ~ op ~ value => Assign(i, value)}

  private def operation: Parser[ExpressionAST] = {
    val opOptions = (OperationPlus | OperationSubtract | OperationEquals)
    val endOp = (opOptions ~ (terminal | funcCall)) ^^ {case _ ~ t => t}

    (funcCall | terminal) ~ opOptions ~ (endOp | expr) ^^ {
      case i ~ op ~ ex => op match {
        case OperationPlus     => Operation("Add", i, ex)
        case OperationSubtract => Operation("Sub", i, ex)
        case OperationEquals   => Operation("Eq", i, ex)
      }
    }
  }

  private def arg: Parser[ExpressionAST] = (SeparatorToken ~ expr) ^^ {case _ ~ e => e}

  private def funcCall: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ expr ~ (arg.*) ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ ex1 ~ rest ~ _ => FuncCall(i, ex1 :: rest)
    }

  private def expr: Parser[ExpressionAST] =
    operation | asign | funcCall | terminal

  private def step: Parser[ExpressionAST] =
    (terminal ~ funcCall ~ AssignOpToken ~ terminal) ^^ {
      case depends ~ definition ~ _ ~ name => Transformation(depends.toString, definition, name.toString)
    }

  private def block: Parser[ExpressionAST] = (step ~ step.*) ^^ {
    case x ~ xs => Blocks(x::xs)
  }
  private def program: Parser[ExpressionAST] = phrase(block)

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
      println("inputs -> " + deps);
      print(List.fill(indent)("\t").mkString);
      printAST(indent+1, definition);
      print(List.fill(indent)("\t").mkString);
      println("output_name -> " + output);
    }
    case Blocks(transforms) => transforms.map(elem => printAST(indent, elem))
  }



  val test = test4

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
dep window(over(dummy),
	asc(sk, true),
	argument1 = lag(title,1)+'-'+last(title),
	lead(title,1)+'-'+last(title),
        output(sk = long),
	startAt: 1) ~> outputname
"""

  lazy val test2: String = 
  """
source1 keyGenerate(output(sk = long),
	startAt: 1) ~> SurrogateKey1
SurrogateKey1 derive(dummy = 1) ~> DerivedColumn1
DerivedColumn1 window(over(dummy),
	asc(sk, true),
	prevAndCurr = lag(title,1)+'-'+last(title),
		nextAndCurr = lead(title,1)+'-'+last(title)) ~> leadAndLag
"""

  lazy val test3: String =
    """
aux window(over+3+f(2)+g(0)+1) ~> outputname
"""

  lazy val test4: String =
    """
1+2-3 == 0
"""
}
