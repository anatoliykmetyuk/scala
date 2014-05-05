package subscript.akka

import subscript.DSL._
import scala.collection.mutable.ListBuffer
import subscript.vm._
import akka.actor._


trait SubScriptActor extends Actor {
  
  val runner: SubScriptActorRunner = SSARunnerV1Scheduler
  
  private object Terminator { // TBD: find better name; something with Blocker
    var executor: EventHandlingCodeFragmentExecutor[N_atomic_action] = null
    
    def script block = @{executor = new EventHandlingCodeFragmentExecutor(there, there.scriptExecutor)}: {. .}
    def release = executor.executeMatching(isMatching=true)
  }
  
  private val callHandlers = ListBuffer[PartialFunction[Any, Unit]]()

  
  
  // Scripts
  def _live(): Script[Unit]
  private def script terminate = Terminator.block
  private def script die       = {if (context ne null) context stop self}
  
  def script r$(handler: Actor.Receive) = @{initForReceive(there, handler)}: {. .}
  
  
  // Callbacks
  override def aroundPreStart() {
    def script lifecycle = (live || terminate) ; die
    runner.launch([lifecycle])
    super.aroundPreStart()
  } 
  
  override def aroundReceive(receive: Actor.Receive, msg: Any) {    
    synchronized {
      sendSynchronizationMessage(this)
      wait()
    }
    
    var messageWasHandled = false
    callHandlers.synchronized {
      for (h <- callHandlers if !messageWasHandled && (h isDefinedAt msg)) {
        h(msg)  // TBD: check for success
        messageWasHandled = true
      }
    }
    
    // If a message was handled, Akka will try to match it against a function that can handle any message
    // otherwise it will try to match the message against function that can handle virtually nothing
    // (except LocalObject, which is certainly local and can't be available from the outside)
    case object LocalObject
    super.aroundReceive(if (messageWasHandled) {case _: Any =>} else {case LocalObject =>}, msg)
  }
  
  override def aroundPostStop() {
    Terminator.release
    super.aroundPostStop()
  }
  
  final def receive: Actor.Receive = {case _ =>} 
  
  // SubScript actor convenience methods
  def initForReceive(node: N_code_eventhandling, _handler: PartialFunction[Any, Unit]) {
    node.codeExecutor = EventHandlingCodeFragmentExecutor(node, node.scriptExecutor)
    val handler = _handler andThen {_ => node.codeExecutor.executeAA}
    synchronized {callHandlers += handler}
    node.onDeactivate {
      synchronized {callHandlers -= handler}
    }
  }
    
  def sendSynchronizationMessage(lock: AnyRef) {
    val vm = runner.executor
    vm insert SynchronizationMessage(vm.rootNode, lock)    
  }

}

case class SynchronizationMessage(node: CallGraphNodeTrait, lock: AnyRef) extends CallGraphMessageN {
  type N = CallGraphNodeTrait
  override def priority = PRIORITY_InvokeFromET - 1  // After all actors are launched
}