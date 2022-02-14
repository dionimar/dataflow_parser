package DataflowParser.ScriptTree

sealed trait ScriptTree

case class ScriptSource(name: String, outputs: List[String]) extends ScriptTree
case class ScriptTrans(inputs: List[String], name: String, outputs: List[String]) extends ScriptTree
case class ScriptSink(inputs: List[String], name: String, outputs: List[String]) extends ScriptTree
