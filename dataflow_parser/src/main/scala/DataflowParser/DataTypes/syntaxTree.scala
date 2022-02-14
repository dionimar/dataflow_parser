package DataflowParser.SyntaxTree

sealed trait OperatorFunc
case object Sub extends OperatorFunc
case object Add extends OperatorFunc
case object Eqq extends OperatorFunc
case object And extends OperatorFunc
case object Or  extends OperatorFunc
case object LEq extends OperatorFunc
case object GEq extends OperatorFunc
case object Great extends OperatorFunc
case object Less extends OperatorFunc
case object Prod extends OperatorFunc
case object Div extends OperatorFunc
case object Mod extends OperatorFunc

sealed trait ExpressionAST
case object EmptyAST extends ExpressionAST
case class Id(value: String) extends ExpressionAST
case class Number(value: Float) extends ExpressionAST
case class Assign(id: ExpressionAST, value: ExpressionAST) extends ExpressionAST
case class Operation(op: OperatorFunc, arg1: ExpressionAST, arg2: ExpressionAST) extends ExpressionAST
case class FuncCall(func: ExpressionAST, args: List[ExpressionAST]) extends ExpressionAST
case class Transformation(depends: List[ExpressionAST], definition: ExpressionAST, output: ExpressionAST) extends ExpressionAST
case class Blocks(transformations: List[ExpressionAST]) extends ExpressionAST
