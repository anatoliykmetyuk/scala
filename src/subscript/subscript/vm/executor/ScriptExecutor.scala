package subscript.vm.executor

import subscript.vm._
import subscript.vm.executor.data._
import subscript.vm.executor.parts._
import subscript.DSL._
import scala.collection.mutable.Buffer
import subscript.vm.executor.data.MessageHandlers.MessageHandler

object ScriptExecutorFactory {
  var scriptDebuggerQueue = new scala.collection.mutable.Queue[MsgListener]
  def addScriptDebugger(sd: MsgListener) = {
    //println("addScriptDebugger: "+sd.getClass.getCanonicalName)
    scriptDebuggerQueue += sd
  }
  def createScriptExecutor(allowDebugger: Boolean) = {
    val se = new CommonScriptExecutor
    if (allowDebugger && !scriptDebuggerQueue.isEmpty) {
      val h = scriptDebuggerQueue.head
      //println("createScriptExecutor: "+se+ " Debugger: "+h.getClass.getCanonicalName)
      scriptDebuggerQueue = scriptDebuggerQueue.tail
      h.attach(se)
    }
    //else println("createScriptExecutor: "+se+" allowDebugger: "+allowDebugger+" scriptDebuggerQueue.isEmpty: "+scriptDebuggerQueue.isEmpty)
    se
  }
}

trait ScriptExecutor extends MsgPublusher with TaskSupplyInterface with Tracer with OldApi {
  // Internal state
  // TBD: change restriction from `subscript` to `vm`
  protected[subscript] val stateAccessLock = new Object
  protected[subscript] val msgQueue        = new MessageQueue   (stateAccessLock) with MQExtras with TrackToBeExecuted
  protected[subscript] val msgHandlers     = new MessageHandlers(stateAccessLock)
  protected[subscript] val graph           = new Graph(this)
  
  def hasSuccess: Boolean = graph.rootNode.hasSuccess
  def hasActiveProcesses = !graph.rootNode.children.isEmpty
  
  // Initialization
  graph.init()
  
  // Lifecycle methods
  /**
   * Launches this VM to execute given script.
   */
  def run(s: Script[_]): ScriptExecutor
  
  /**
   * Performs initialization before this VM starts working.
   * Must be called before VM starts operating.
   * Must be called exactly once.
   */
  def initializeExecution(s: Script[_]): Unit
  
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

abstract class AbstractScriptExecutor extends ScriptExecutor {
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
  msgHandlers sInsert defaultHandler
  msgHandlers sInsert communicationHandler
  
  def run(s: Script[_]) = {
    initializeExecution(s)
    while (hasActiveProcesses) {
      updateCollections()
      if (tryHandleMessage(Int.MinValue)==null) awaitMessages
    }
    this
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

/**
 * This is for compatibility with not yet refactored part of the VM.
 */
trait OldApi {this: ScriptExecutor =>
  def insert(m: CallGraphMessage) = msgQueue sInsert m  
  def rootNode = graph.rootNode
  def addHandler(h: MessageHandler) = msgHandlers sInsert h
}
