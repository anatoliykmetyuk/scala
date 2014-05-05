package subscript.vm.executor

import scala.collection.mutable.ListBuffer
import scala.concurrent.Lock

/**
 * Represents an I/O operation on collection.
 */
sealed trait Operation[+A]
case class Insert[A](e: A) extends Operation[A]
case class Remove[A](e: A) extends Operation[A]

/**
 * This trait represents a collection with unsafe (fast) and safe (slow)
 * access modes. Safety implies synchronization.
 * It is supposed to be used in unsafe way from only one thread;
 * all other threads should access it in safe manner.
 */
trait SemiSafeMutableCollection[T[A] <: Traversable[A], +A] {
  
  /* INTERNAL STATE */
  /**
   * Underlying collection. Naturally, unsafe (or, more precisely, with native
   * safety settings), if used directly.
   */
  val collection: T[A]
  
  /**
   * This is thread-safety stragegy implementation.
   */
  val lock: AnyRef
  
  /**
   * Buffer records changes that are meant for the collection. The changes
   * are stored in chronological order in this format.
   */
  private val buffer = ListBuffer[Operation[A]]()
  
  
  /* UNSAFE OPERATIONS */
  /**
   * Adds an element to the collection, optionally with key specified. If the collection
   * doesn't support keys, key should be None.
   */
  def add(e: A): Unit
  
  /**
   * Removes specified element from a collection.
   * @return true, if the element was found.
   */
  def remove(x: A): Unit = unsupportedOperation
  
  /**
   * This operation commits recorded changes into the underlying collection.
   * Also, it clears buffer. Synchronization operation is not safe, because it calls
   * unsafe operations. 
   */
  def commit() = {
    buffer.foreach {
      case Insert(e) => add   (e)
      case Remove(e) => remove(e)
    }
    buffer.clear()
  }
  
  
  /* SAFE OPERATIONS */
  /**
   * Safe task execution.
   */
  def safe[R](f: => R): R = lock.synchronized(f)
  
  def safeAdd   (e: A) = safe {buffer += Insert(e)}
  def safeRemove(e: A) = safe {buffer += Remove(e)}
  
  
  /* HELPERS */
  protected def unsupportedOperation = throw new UnsupportedOperationException("This operation is not supported")
  
}