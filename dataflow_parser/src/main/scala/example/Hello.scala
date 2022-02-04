package example

import scala.util.parsing.combinator._
import scala.util.matching.Regex



sealed trait DataflowToken
case class IdentifierToken(s: String) extends DataflowToken
case object LeftParenToken extends DataflowToken
case object RightParenToken extends DataflowToken
case object SeparatorToken extends DataflowToken
case object LeftBraceToken extends DataflowToken
case object RightBraceToken extends DataflowToken

// case class ComponentsToken(s: String) extends DataflowToken
// case object JoinToken extends DataflowToken
// case object SplitToken extends DataflowToken
// case object ExistsToken extends DataflowToken
// case object UnionToken extends DataflowToken
// case object LookupToken extends DataflowToken
// case object DeriveToken extends DataflowToken
// case object SelectToken extends DataflowToken
// case object AggregateToken extends DataflowToken
// case object KeyGenerateToken extends DataflowToken
// case object PivotToken extends DataflowToken
// case object WindowToken extends DataflowToken
// case object RankToken extends DataflowToken
// case object CallToken extends DataflowToken
// case object FoldDownToken extends DataflowToken
// case object ParseToken extends DataflowToken
// case object StringifyToken extends DataflowToken
// case object FilterToken extends DataflowToken
// case object SortToken extends DataflowToken
// case object AlterRowToken extends DataflowToken
// case object AssertToken extends DataflowToken
// case object ComposeToken extends DataflowToken // flowlets
// case object SourceToken extends DataflowToken
// case object SinkToken extends DataflowToken
// case object AssignToken extends DataflowToken



object ScriptLexer extends RegexParsers {

  override def skipWhitespace = true
  //override def whiteSpace: Regex = "[ \t\r\f\n]+".r

  def identifierToken: Parser[DataflowToken] = """[a-zA-Z0-9]+""".r ^^ (id => IdentifierToken(id))
  def leftParenToken: Parser[DataflowToken] = "(" ^^ (_ => LeftParenToken)
  def rightParenToken: Parser[DataflowToken] = ")" ^^ (_ => RightParenToken)
  def leftBraceToken: Parser[DataflowToken] = "{" ^^ (_ => LeftBraceToken)
  def rightBraceToken: Parser[DataflowToken] = "}" ^^ (_ => RightBraceToken)
  def separatorToken: Parser[DataflowToken] = "," ^^ (_ => SeparatorToken)

  // def assignToken: Parser[DataflowToken] = """\\~\\>""" ^^ (_ => AssignToken)
  // def joinToken: Parser[DataflowToken] = """join""" ^^ (_ => JoinToken)
  // def splitToken: Parser[DataflowToken] = """split""" ^^ (_ => SplitToken)
  // def existsToken: Parser[DataflowToken] = """exists""" ^^ (_ => ExistsToken)
  // def unionToken: Parser[DataflowToken] = """union""" ^^ (_ => UnionToken)
  // def lookupToken: Parser[DataflowToken] = """lookup""" ^^ (_ => LookupToken)
  // def deriveToken: Parser[DataflowToken] = """derive""" ^^ (_ => DeriveToken)
  // def selectToken: Parser[DataflowToken] = """select""" ^^ (_ => SelectToken)
  // def aggregateToken: Parser[DataflowToken] = """aggregate""" ^^ (_ => AggregateToken)
  // def keyGenerateToken: Parser[DataflowToken] = """keyGenerate""" ^^ (_ => KeyGenerateToken)
  // def pivotToken: Parser[DataflowToken] = """pivot""" ^^ (_ => PivotToken)
  // def windowToken: Parser[DataflowToken] = """window""" ^^ (_ => WindowToken)
  // def rankToken: Parser[DataflowToken] = """rank""" ^^ (_ => RankToken)
  // def callToken: Parser[DataflowToken] = """call""" ^^ (_ => CallToken)
  // def foldDownToken: Parser[DataflowToken] = """foldDown""" ^^ (_ => FoldDownToken)
  // def parseToken: Parser[DataflowToken] = """parse""" ^^ (_ => ParseToken)
  // def stringifyToken: Parser[DataflowToken] = """stringify""" ^^ (_ => StringifyToken)
  // def filterToken: Parser[DataflowToken] = """filter""" ^^ (_ => FilterToken)
  // def sortToken: Parser[DataflowToken] = """sort""" ^^ (_ => SortToken)
  // def alterRowToken: Parser[DataflowToken] = """alterRow""" ^^ (_ => AlterRowToken)
  // def assertToken: Parser[DataflowToken] = """assert""" ^^ (_ => AssertToken)
  // def composeToken: Parser[DataflowToken] = """compose""" ^^ (_ => ComposeToken)
  // def sourceToken: Parser[DataflowToken] = "source(" ^^ (_ => SourceToken)
  // def sinkToken: Parser[DataflowToken] = "sink(" ^^ (_ => SinkToken)
   
  private def tokens: Parser[List[DataflowToken]] =
    phrase(
      rep1(identifierToken |
        leftParenToken |
        rightParenToken|
        separatorToken
      )
    )

  def tokenize(script: String): ParseResult[List[DataflowToken]] = parse(tokens, script)
}







object Hello extends Greeting with App {
  println(ScriptLexer.tokenize(greeting))
}

trait Greeting {
  lazy val greeting: String = "hello word(hola, adios)"
}
