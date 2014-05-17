package subscript.vm.executor.parts

import subscript.vm._

/**
 * Notifies given lock when a message gets queued
 */
class MessageQueuedNotifier(lock: AnyRef) extends MsgListener {  
  override def messageQueued(m: CallGraphMessage) = {
    lock.synchronized { lock.notify() }
  }
}