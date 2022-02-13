package DataflowParser.Tokens

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

sealed trait OperatorToken extends DataflowToken
case object OperationPlus extends OperatorToken
case object OperationEquals extends OperatorToken
case object OperationSubtract extends OperatorToken
