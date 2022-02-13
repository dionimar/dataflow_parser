package DataflowParser.Parser

import scala.util.parsing.combinator._

import DataflowParser.Tokens._
import DataflowParser.SyntaxTree._

import DataflowParser.Lexer._



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
        case OperationPlus     => Operation(Add, i, ex)
        case OperationSubtract => Operation(Sub, i, ex)
        case OperationEquals   => Operation(Eqq, i, ex)
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
    program(reader)
  }
}
