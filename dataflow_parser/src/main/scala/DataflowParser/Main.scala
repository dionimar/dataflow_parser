package DataflowParser

import DataflowParser.Lexer.ScriptLexer
import DataflowParser.Parser.ExpressionParser




object TreePrinter {
  import DataflowParser.SyntaxTree._

  def printAST(indent: Integer)(ast: ExpressionAST): Unit = {
    def identFun(out: String): Unit = println(List.fill(indent)("  ").mkString + out)
    ast match {
      case Id(name) => identFun(name)
      case Number(n) => identFun(n.toString)
      case Assign(id, expr) => {
        identFun(id)
        printAST(indent + 1)(expr)
      }
      case Operation(op, ex1, ex2) => {
        identFun(op.toString)
        printAST(indent + 1)(ex1)
        printAST(indent + 1)(ex2)
      }
      case FuncCall(id, args) => {
        identFun("call " + id.toString)
        args.map(x => printAST(indent + 1)(x))
      }
      case Transformation(deps, definition, output) => {
        identFun("inputs -> " + deps);
        printAST(indent + 1)(definition);
        identFun("output_name -> " + output);
      }
      case Blocks(transforms) => transforms.map(elem => printAST(indent)(elem))
    }
  }
}






object StringParser extends Inputs with App {
  val test = test2

  println(test)
  val tokens = ScriptLexer.tokenize(test).toOption

  //println(tokens)


  import DataflowParser.SyntaxTree._

  val transformations = tokens
    .flatMap(ExpressionParser.parseFromTokens(_).toOption)
    .map(x => x match {case Blocks(trans) => trans})

  transformations
    .map(_.map(
      _ match {
        case Transformation(dep, _, out) => println(dep + " -> Some operations -> " + out)
      }
    ))
    

  // tokens match {
  //   case Left(msg) => println(msg)
  //   case Right(listTokens) => 
  //     ExpressionParser.parseFromTokens(listTokens) match {
  //       case Left(msg) => println(msg)
  //       case Right(syntaxAST) => 
  //     }
      
    
  // }
  
}






trait Inputs {
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
aux window(over+3+f(2)+g(0)-1) ~> outputname
"""

  lazy val test4: String =
    """
aux f(1+2-3 == 0 || 1 && d) ~> aux
"""
}
