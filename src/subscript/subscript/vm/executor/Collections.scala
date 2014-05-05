package subscript.vm.executor

import scala.collection.mutable.{PriorityQueue, ListBuffer}

import subscript.vm._
import ScriptExecutorTools._
import subscript.DSL._

class MessageQueue(val lock: AnyRef) extends SemiSafeMutableCollection[PriorityQueue, CallGraphMessage] {

  val collection = PriorityQueue[CallGraphMessage]()(new Ordering[CallGraphMessage] {
    def compare(x: CallGraphMessage, y: CallGraphMessage): Int = {
      var p = x.         priority - y.         priority; if (p != 0) return p 
          p = x.secondaryPriority - y.secondaryPriority; if (p != 0) return p
      return  x. tertiaryPriority - y. tertiaryPriority
    }
  })
    
  def add(e: CallGraphMessage): Unit = collection += e
    
  def dequeue: Option[CallGraphMessage] = if (collection.isEmpty) None else Some(collection.dequeue)
  
}

class MessageHandlers(val lock: AnyRef) extends SemiSafeMutableCollection[ListBuffer, MessageHandler] {
  
  val collection = ListBuffer[MessageHandler]()
  
  def add(h: MessageHandler): Unit = collection += h
    
  def remove(h: MessageHandler): Unit = collection -= h
  
}

object Graph {
  def newInstance(): Graph = ???
}
class Graph(val rootNode: N_launch_anchor)