package subscript.akka

import subscript.DSL._
import scala.collection.mutable.ListBuffer
import subscript.vm._
import akka.actor._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.language.postfixOps


trait SubScriptActorRunner {
  
  def launch(s: Script[_])
  
  def execute(debugger: MsgListener)
  
  def system: ActorSystem
 
  def executor: CommonScriptExecutor
  
}

object SSARunnerV1Scheduler extends SubScriptActorRunner {
  
  // Don't do heavy operations until needed
  lazy val system = ActorSystem()
  lazy val executor = ScriptExecutorFactory.createScriptExecutor(true)
  
  var launch_anchor: N_launch_anchor = null
  
  def scheduledTaskDelay = 1 milliseconds
  
  def launch(s: Script[_]) = executor.invokeFromET {launch_anchor.launch(s)}
  
  def script..
     // make a local anchor for launched actor processes, 
     // so that we will be able to kill those here using the || and / operators
     live = @{launch_anchor=there}: (** {. .} **)

  def execute(debugger: MsgListener) {
    if (debugger!=null) debugger.attach(executor)
    executor.addHandler(synchMsgHandler)
    executor.initializeExecution(_live())
    doScriptSteps_loop
  }

  def doScriptSteps_loop: Unit = {
    doScriptSteps
    if (executor.hasActiveProcesses) {
      try system.scheduler.scheduleOnce(scheduledTaskDelay)(doScriptSteps_loop)
      catch {case _: IllegalStateException =>}  // Ignore certain things...
    }
  }

  def doScriptSteps = {    
    var handledMessage = executor.tryHandleMessage(minimalPriorityForAA = Int.MinValue)
    while (handledMessage!=null)
      handledMessage = executor.tryHandleMessage(minimalPriorityForAA = Int.MinValue)
    executor.messageAwaiting
  }
  
  val synchMsgHandler: PartialFunction[CallGraphMessage, Unit] = {
    case SynchronizationMessage(_, lock) => lock.synchronized(lock.notify())
  }

}