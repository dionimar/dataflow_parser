package DataflowParser.Parser

import scala.util.parsing.combinator._

import DataflowParser.Tokens._
import DataflowParser.SyntaxTree._

import DataflowParser.Lexer._
import scala.util.Failure



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
    (id ~ AssignEqToken ~ (expr | terminal)) ^^ {case Id(i) ~ op ~ value => Assign(Id(i), value)}

  private def operation: Parser[ExpressionAST] = {
    val opOptions =
      (OperationPlus | OperationSubtract | OperationEquals | OperationAnd | OperationOr
        | OperationDiv | OperationProd | OperationMod
      )

    val endOp = (opOptions ~ (terminal | funcCall)) ^^ {case _ ~ t => t}

    opt(funcCall | terminal) ~ opOptions ~ (endOp | expr) ^^ {
      case Some(i) ~ op ~ ex => op match {
        case OperationPlus     => Operation(Add, i, ex)
        case OperationSubtract => Operation(Sub, i, ex)
        case OperationLEq      => Operation(LEq, i, ex)
        case OperationGEq      => Operation(GEq, i, ex)
        case OperationLess     => Operation(Less, i, ex)
        case OperationGreat    => Operation(Great, i, ex)
        case OperationEquals   => Operation(Eqq, i, ex)
        case OperationOr       => Operation(Or, i, ex)
        case OperationDiv      => Operation(Div, i, ex)
        case OperationProd     => Operation(Prod, i, ex)
        case OperationMod      => Operation(Mod, i, ex)
        case OperationAnd      => Operation(And, i, ex)}
      case None ~ OperationSubtract ~ ex => Operation(Sub, Number(0), ex)
    }
  }

  private def arg: Parser[ExpressionAST] = (SeparatorToken ~ expr) ^^ {case _ ~ e => e}

  private def funcCallWithoutArgs: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ _ => FuncCall(Id(i), List())
    }

  private def funcCallWithArgs: Parser[ExpressionAST] =
    (id ~ LeftParenToken ~ expr ~ (arg.*) ~ RightParenToken) ^^ {
      case Id(i) ~ _ ~ ex1 ~ rest ~ _ => FuncCall(Id(i), ex1 :: rest)
    }

  private def funcCall: Parser[ExpressionAST] = (funcCallWithArgs | funcCallWithoutArgs)

  private def expr: Parser[ExpressionAST] =
    operation | asign | funcCall | terminal


  private def inputStep: Parser[ExpressionAST] ={   
    (funcCall ~ AssignOpToken ~ terminal) ^^ {
      case definition ~ _ ~ name => Transformation(List(EmptyAST), definition, name)
    }
  }

  private def fullStep: Parser[ExpressionAST] ={
    val multipleArgs = (SeparatorToken ~> terminal).*
    val arguments = terminal ~ multipleArgs
    
    (arguments ~ funcCall ~ AssignOpToken ~ terminal) ^^ {
      case depends ~ rest ~ definition ~ _ ~ name => Transformation(depends::rest, definition, name)
    }
  }

  private def step: Parser[ExpressionAST] = (fullStep | inputStep)

  private def block: Parser[ExpressionAST] = (step ~ step.*) ^^ {
    case x ~ xs => Blocks(x::xs)
  }
  private def program: Parser[ExpressionAST] = phrase(block)

  def parseFromTokens(input: List[DataflowToken]): Either[String, ExpressionAST] = {
    val reader = new ExpressionTokenReader(input)
    program(reader) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)
    }
  }
}
