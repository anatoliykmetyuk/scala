package subscript.vm.executor

import subscript.vm.CallGraphMessage
import subscript.DSL._
import subscript.vm.CallGraphMessage

object ScriptExecutorTools {

  type MessageHandler = PartialFunction[CallGraphMessage, Unit]
  
}

trait ScriptExecutor {
  
  val stateAccessLock = new Object
  val msgQueue        = new MessageQueue   (stateAccessLock)
  val msgHandlers     = new MessageHandlers(stateAccessLock)
  val graph           = Graph.newInstance
  
  def run(s: Script[_]): Unit
  
  def initializeExecution(s: Script[_]): Unit
  
  def handleMessage(msg: CallGraphMessage): Unit =
    for (h <- msgHandlers.collection if h isDefinedAt msg) h(msg)
  
  def hasActiveProcesses: Boolean
  
}