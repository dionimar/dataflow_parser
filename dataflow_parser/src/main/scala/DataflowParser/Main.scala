package DataflowParser

import DataflowParser.Lexer.ScriptLexer
import DataflowParser.Parser.ExpressionParser




object TreePrinter {
  import DataflowParser.SyntaxTree._

  def printAST(indent: Integer)(ast: ExpressionAST): Unit = {
    def identFun(out: String): Unit =  println(List.fill(indent)("  ").mkString + out)
    ast match {
      case Id(name) => identFun(name)
      case Number(n) => identFun(n.toString)
      case Assign(id, expr) => {
        identFun(id + " <- ")
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
        identFun("inputs -> " + deps)
        printAST(indent + 1)(definition)
        identFun("output_name -> " + output)
      }
      case Blocks(transforms) => transforms.map(elem => printAST(indent)(elem))
    }
  }
}






object StringParser extends Inputs with App {




  def performTest(test: String) = {
    println(test)
    val tokens = ScriptLexer.tokenize(test).toOption
    println(tokens)

    import DataflowParser.SyntaxTree._

    val transformations = tokens
      .flatMap(ExpressionParser.parseFromTokens(_).toOption)
      .map(x => x match {case Blocks(trans) => trans})
    println(transformations)
    transformations.map(_.map(TreePrinter.printAST(0)(_)))
  }

  //List(test_2).map(performTest)
  List(
    //test1
    // test2,
    test3
    // test_1,
    //test_2
    // test_3,
    // test_4,
    // test_5
  ).map(performTest)

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
a, b, c keyGenerate(output(sk = long),
	startAt: 1) ~> SurrogateKey1
SurrogateKey1 derive(dummy = 1) ~> DerivedColumn1
DerivedColumn1 window(over(dummy),
	asc(sk, true),
	prevAndCurr = lag(title,1)+'-'+last(title),
		nextAndCurr = lead(title,1)+'-'+last(title)) ~> leadAndLag
"""

  lazy val test3: String =
    """
ource(output(
		movieId as string,
		title as string,
		genres as string
	),
	allowSchemaDrift: true,
	validateSchema: false) ~> source1
source1 derive(upperCaseTitle = upper(title)) ~> deriveTransformationName
source1 sink(allowSchemaDrift: true,
	validateSchema: false) ~> sink1
"""

  lazy val test4: String =
    """
a f(1) ~> aux
a, b f(1) ~> aux
f(1) ~> aux
"""



  lazy val test_1: String =
        """
f() ~> aux
"""

  lazy val test_2: String =
        """
a f(2-1) ~> aux
a f(-1) ~> aux
"""

  lazy val test_3: String =
        """
a, b f(1) ~> aux
"""


  lazy val test_4 =
    """ 
Join2 derive(edad = abs(toInterger(divide(mothsBetween({b.neo_fechadenacimiento}, currentDate()), 12)))) ~> DerviedColumn2
"""
  lazy val test_5 = """
DerivedColumn2 derive(Grupo_Edad = case(
    edad < 18, 1, 
    edad, 18, 0, -1
), 
Junta_Provincial = iif(isNull({pro.neo_codigo}), -1, toInteger({pro.neo_codigo})),
Beneficiario = {b.beneficiario} + '_ADA/Respirapp'
) ~> DerivedColumn3
"""
}
