package DataflowParser.SyntaxTree


sealed trait OperatorFunc
case object Sub extends OperatorFunc
case object Add extends OperatorFunc
case object Eqq extends OperatorFunc

sealed trait ExpressionAST
case class Id(value: String) extends ExpressionAST
case class Number(value: Float) extends ExpressionAST
case class Assign(id: String, value: ExpressionAST) extends ExpressionAST
case class Operation(op: OperatorFunc, arg1: ExpressionAST, arg2: ExpressionAST) extends ExpressionAST
case class FuncCall(func: String, args: List[ExpressionAST]) extends ExpressionAST
case class Transformation(depends: String, definition: ExpressionAST, output: String) extends ExpressionAST
case class Blocks(transformations: List[ExpressionAST]) extends ExpressionAST
