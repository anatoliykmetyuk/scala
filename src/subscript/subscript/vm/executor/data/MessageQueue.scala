package subscript.vm.executor.data

import scala.collection.mutable.PriorityQueue
import subscript.vm._
import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._

class MessageQueue(val lock: AnyRef) extends SafeCollection[CallGraphMessage] with MsgPublisher with MessagePriorities {self =>
  private case class Enqueue(msg: CallGraphMessage) extends Operation {def commit = self enqueue msg}
  
  /* Internal state */
  val ordering = new Ordering[CallGraphMessage] {
    def compare(x: CallGraphMessage, y: CallGraphMessage): Int = {
      var p = x.         priority - y.         priority; if (p != 0) return p 
          p = x.secondaryPriority - y.secondaryPriority; if (p != 0) return p
      return  x. tertiaryPriority - y. tertiaryPriority
    }
  }
  val collection = PriorityQueue[CallGraphMessage]()(ordering)
  
  private var _nMessages = 0
  private def nextMessageID = {_nMessages+=1; _nMessages}
  
  
  /* Public unsafe API */
  def insert(e: CallGraphMessage): Unit = {
    e.index = nextMessageID
    enqueue(e)
    messageQueued(e)
  }
 
  def enqueue(e: CallGraphMessage): Unit = collection += e
  
  def remove(m: CallGraphMessage) = {
    messageDequeued(m)
    //scriptGraphMessages -= m  is not allowed...FTTB we will ignore this message, by checking the canceled flag in the executor
  }
 
  def dequeue(minimalPriorityForAA: Int): CallGraphMessage = {
    if (collection.isEmpty) return null
    
    if (minimalPriorityForAA > Int.MinValue) {
      val h = collection.head
      if (h.priority <= PRIORITY_AAToBeExecuted) {
        h match {
          case aatbe@AAToBeExecuted(n: N_atomic_action[_]) if (n.priority >= minimalPriorityForAA) =>
          case _ => return null
        }
      }
    }
    
    val result = collection.dequeue
    //if (callGraphMessageCount != callGraphMessages.length) {
    //  println("dequeue: "+callGraphMessageCount+" != " + callGraphMessages.length)
    //}
    messageDequeued(result)
    result
  }
  
  
  /* Pubic safe API */
  def sEnqueue(e: CallGraphMessage): Unit = this push Enqueue(e)

}

/**
 * Provides convenience methods to quickly insert certain messages.
 */
trait MQExtras {this: MessageQueue =>
  
  def doNeutral(n: CallGraphNodeTrait) =
    if (n.getLogicalKind_n_ary_op_ancestor!=LogicalKind.Or) insert(Success(n))
  
  def insertDeactivation(n:CallGraphNodeTrait,c:CallGraphNodeTrait) = insert(Deactivation(n, c, false))
  
  def insertContinuation(message: CallGraphMessage, child: CallGraphTreeNode = null): Unit = {
    val n = message.node.asInstanceOf[ N_n_ary_op]
    var c = n.continuation 
    
    // Continuations are merged with already existing ones
    // TBD: make separate priorities of continuations...
    // e.g. a continuation for AAActivated should not be merged (probably) with one for AAHappened
    if (c==null) {
      c = new Continuation(n)
    }
    if (c.childNode==null) // should be improved
    {
      c.childNode = 
        if (child!=null) child
        else message match {
          case Break(an,c,m) => c
          case Success(an,c) => c
          case _ => message.node
        }
    }
    message match {
      case a@ Activation  (node: CallGraphNodeTrait) => c.activation = a
      case a@Deactivation (node: CallGraphNodeTrait,
                          child: CallGraphNodeTrait, excluded: Boolean) => c.deactivations ::= a
      case a@Success      (node: CallGraphNodeTrait,
                          child: CallGraphNodeTrait)  => c.success = a
      case a@Break        (node: CallGraphNodeTrait, 
                          child: CallGraphNodeTrait, 
                 activationMode: ActivationMode.ActivationModeType)  => c.break = a
      case a@AAActivated  (node: CallGraphNodeTrait, 
                          child: CallGraphNodeTrait) =>  c.aaActivated = a
      case a@CAActivated  (node: CallGraphNodeTrait, 
                          child: CallGraphNodeTrait) =>  c.caActivated = a
      case a@AAHappened   (node: CallGraphNodeTrait, 
                          child: CallGraphNodeTrait, _) =>  c.aaHappeneds ::= a
    }
    if (n.continuation==null) {
       n.continuation = c
       insert(c)
    }
    else messageContinuation(message, c)
  }
  // insert a continuation message for a unary operator
  def insertContinuation1(message: CallGraphMessage): Unit = {
    val n = message.node.asInstanceOf[N_1_ary_op]
    var c = n.continuation
    if (c==null) {
      c = new Continuation1(n)
      n.continuation = c
      enqueue(c)
    }
    enqueue(Continuation1(n))
  }
}

trait TrackToBeExecuted extends MessageQueue {
  override def insert(m: CallGraphMessage) {
    super.insert(m)
    track(m, true)
  }
  override def remove(m: CallGraphMessage) {
    super.remove(m)
    track(m, false)
  }
  override def dequeue(minimalPriorityForAA: Int) = {
    val msg = super.dequeue(minimalPriorityForAA)
    if (msg ne null) track(msg, false)
    msg
  }
  
  private def track(m: CallGraphMessage, track: Boolean) {
    // AA nodes keep track of associated "to be executed" messages.
    // This way, if such a node is excluded by another process, it will be able to get such a message out of the queue;
    // then that message may be garbage collected and the link to the node will be gone, so that the node may also 
    // be garbage collected
    m match {
      case maa@AAToBeExecuted  (n: N_atomic_action[_]) => n.msgAAToBeExecuted = if (track) maa else null
      case maa@AAToBeReexecuted(n: N_atomic_action[_]) => n.msgAAToBeExecuted = if (track) maa else null
      case _ =>
    }
  }
}