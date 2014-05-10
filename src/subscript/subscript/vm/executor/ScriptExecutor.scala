package subscript.vm.executor

import subscript.vm._
import subscript.vm.executor.data._
import subscript.vm.executor.parts._
import subscript.DSL._
import scala.collection.mutable.Buffer

trait ScriptExecutor {
  // Internal state
  protected[vm] val stateAccessLock = new Object
  protected[vm] val msgQueue        = new MessageQueue   (stateAccessLock) with MQExtras with TrackToBeExecuted
  protected[vm] val msgHandlers     = new MessageHandlers(stateAccessLock)
  protected[vm] val graph           = new Graph(this)  

  def hasSuccess: Boolean = graph.rootNode.hasSuccess
  def hasActiveProcesses = !graph.rootNode.children.isEmpty
  
  // Lifecycle methods
  /**
   * Launches this VM to execute given script.
   */
  def run(s: Script[_]): Unit
  
  /**
   * Performs initialization before this VM starts working.
   * Must be called before VM starts operating.
   * Must be called exactly once.
   */
  def initializeExecution(s: Script[_])
  
  /**
   * Tries to dequeue and handle message from the messages queue.
   */
  def tryHandleMessage(minimalPriorityForAA: Int): CallGraphMessage
  
  /**
   * Applies each registered handler to a given message, if
   * this handler is defined over this message.
   */
  def handle(message: CallGraphMessage): Unit
  
  /**
   * Specifies the logic to be executed when there are no
   * more messages to handle, but they are expected in future
   */
  def awaitMessages: Unit
  
  /**
   * Synchronizes all the collections in a thread-safe manner.
   */
  def updateCollections() = stateAccessLock.synchronized {
    msgQueue   .commit()
    msgHandlers.commit()
  }
}

abstract class AbstractScriptExecutor extends ScriptExecutor with TaskSupplyInterface with MsgPublusher {
  // Initialization
  msgQueue addListener this
  
  // Lifecycle methods  
  /**
   * Performs initialization before this VM starts working.
   * Must be called before VM starts operating.
   * Must be called exactly once.
   */
  def initializeExecution(s: Script[_]) {
    val anchorNode = graph.anchorNode
    s(anchorNode)
    graph.activateFrom(anchorNode, anchorNode.t_callee)
  }
  
  /**
   * Tries to dequeue and handle message from the messages queue.
   */
  def tryHandleMessage(minimalPriorityForAA: Int): CallGraphMessage = {
    val m = msgQueue.dequeue(minimalPriorityForAA)
    if (m == null) return null
    messageHandled(m)
    handle(m)
    m
  }
  
  /**
   * Applies each registered handler to a given message, if
   * this handler is defined over this message.
   */
  def handle(message: CallGraphMessage): Unit =
    for (h <- msgHandlers.collection if h isDefinedAt message) h(message)
}

class CommonScriptExecutor extends AbstractScriptExecutor with Tracer with
    DefaultHandlers {
  msgQueue addListener new MessageQueuedNotifier(this)
  
  def run(s: Script[_]) {
    initializeExecution(s)
    while (hasActiveProcesses) {
      updateCollections()
      if (tryHandleMessage(Int.MinValue)==null) awaitMessages
    }
  }
  
  def awaitMessages {
    messageAwaiting
    synchronized { // TBD: there should also be a synchronized call in the CodeExecutors
      if (msgQueue.collection.size == 0) // looks stupid, but event may have happened&notify() may have been called during tracing
          wait() // for an event to happen 
    }
    // note: there may also be deadlock because of unmatching communications
    // so there should preferably be a check for the existence of waiting event handling actions
  }
}

