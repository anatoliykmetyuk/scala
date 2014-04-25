package subscript.vm

trait ScriptDebugger {
  var scriptExecutor: ScriptExecutor = null
  def messageHandled     (m: CallGraphMessage)
  def messageQueued      (m: CallGraphMessage)
  def messageDequeued    (m: CallGraphMessage)
  def messageContinuation(m: CallGraphMessage, c: Continuation)
  def messageAwaiting

  def attach(se: ScriptExecutor): Unit = {scriptExecutor = se; se.scriptDebugger = this}
  
}