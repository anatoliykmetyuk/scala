/*
    This file is part of Subscript - an extension of the Scala language 
                                     with constructs from Process Algebra.

    Subscript is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License and the 
    GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Subscript consists partly of a "virtual machine". This is a library; 
    Subscript applications may distribute this library under the 
    GNU Lesser General Public License, rather than under the 
    GNU General Public License. This way your applications need not 
    be made Open Source software, in case you don't want to.

    Subscript is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You may have received a copy of the GNU General Public License
    and the GNU Lesser General Public License along with Subscript.
    If not, see <http://www.gnu.org/licenses/>
*/

package subscript.vm
import scala.collection.mutable._

  abstract class AAHappenedMode
  case object AtomicCodeFragmentExecuted    extends AAHappenedMode
  case object DurationalCodeFragmentStarted extends AAHappenedMode
  case object DurationalCodeFragmentEnded   extends AAHappenedMode
  
  trait MessagePriorities {
    val PRIORITY_AAToBeReexecuted             =  0
    val PRIORITY_AAToBeExecuted               =  1
    val PRIORITY_CAActivatedTBD               =  2
    val PRIORITY_CommunicationMatchingMessage =  3
    val PRIORITY_Continuation                 =  4
    val PRIORITY_Continuation1                =  5
    val PRIORITY_AAExecutionFinished          =  6
    val PRIORITY_Deactivation                 =  7
    val PRIORITY_Activation                   =  8
    val PRIORITY_Suspend                      =  9
    val PRIORITY_Resume                       = 10
    val PRIORITY_Exclude                      = 11
    val PRIORITY_Success                      = 12
    val PRIORITY_Break                        = 13
    val PRIORITY_AAActivated                  = 14
    val PRIORITY_CAActivated                  = 15
    val PRIORITY_AAHappened                   = 17
    val PRIORITY_InvokeFromET                 = Int.MaxValue // TBD
  }

  trait CallGraphMessage {
  
    type N <: CallGraphNodeTrait
    
    var index = -1
	def node: N

    def priority: Int     // TBD: determine good priority levels
    def secondaryPriority = -node.index // oldest nodes first
    def  tertiaryPriority = 0
    
	val className = getClass.getSimpleName
    override def toString =    index+" "+className+" "+node
    def toFormattedString = f"$index%3d $className%14s $node"
  }

  // various kinds of messages sent around in the script call graph
  abstract class CallGraphMessageN extends CallGraphMessage with MessagePriorities
  
	case class Activation   (node: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Activation}
	case class Continuation (node:  N_n_ary_op)        extends CallGraphMessageN {
	  type N = N_n_ary_op; 
	  def priority = PRIORITY_Continuation
	  override def secondaryPriority = node.index // newest nodes first 

	  var activation: Activation = null
	  var deactivations: List[Deactivation] = Nil
	  var success: Success = null
	  var break: Break = null
	  var aaActivated: AAActivated = null
	  var caActivated: CAActivated = null
	  var aaHappeneds : List[AAHappened] = Nil
	  var childNode  : CallGraphNodeTrait = null
	  
	  override def toString = {
	    var strs = new ListBuffer[String]
	    if (activation   !=null) strs += activation   .toString
	    if (deactivations!=Nil ) strs += deactivations.mkString
	    if (success      !=null) strs += success      .toString
	    if (break        !=null) strs += break        .toString
	    if (aaActivated  !=null) strs += aaActivated  .toString
	    if (caActivated  !=null) strs += caActivated  .toString
	    if (aaHappeneds  !=Nil ) strs += aaHappeneds  .mkString
	    if (childNode    !=null) strs += childNode    .toString
	    var result = super.toString +
	                 s" {${strs.mkString(", ")}} ${node.infoString}"
	    result
	  }
	}
	case class Continuation1      (node: N_1_ary_op) extends CallGraphMessageN {type N = N_1_ary_op; def priority = PRIORITY_Continuation}
	case class Deactivation       (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait, excluded: Boolean) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Deactivation}
	case class Suspend            (node: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Suspend}
	case class Resume             (node: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Resume}
	case class Exclude          (parent: CallGraphNodeTrait, 
	                               node: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Exclude}
	case class Success            (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait = null) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Success}
	case class Break              (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait, activationMode: ActivationMode.ActivationModeType) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_Break}
	case class AAActivated        (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_AAActivated}
	case class CAActivated        (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_CAActivated} // for immediate handling
	case class CAActivatedTBD     (node: N_call            ) extends CallGraphMessageN {type N = N_call; def priority = PRIORITY_CAActivatedTBD} // for late handling
	case class AAHappened         (node: CallGraphNodeTrait, 
	                              child: CallGraphNodeTrait, mode: AAHappenedMode) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_AAHappened}
	case class AAExecutionFinished(node: CallGraphNodeTrait) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_AAExecutionFinished}
	case class AAToBeExecuted     (node: N_atomic_action   ) extends CallGraphMessageN {type N = N_atomic_action; def priority = PRIORITY_AAToBeExecuted
	  override def secondaryPriority =  node.priority 
	  override def tertiaryPriority  = -node.index // oldest nodes first 
    }
	case class AAToBeReexecuted   (node: N_atomic_action) extends CallGraphMessageN {type N = N_atomic_action; def priority = PRIORITY_AAToBeReexecuted
	  override def secondaryPriority = -index // oldest messages first, so that retries are FIFO  
	}
	case object CommunicationMatchingMessage extends CallGraphMessageN {
	  type N = CallGraphNodeTrait
	  def priority = PRIORITY_CommunicationMatchingMessage 
	  def node:CallGraphNodeTrait = null
	  def activatedCommunicatorCalls = scala.collection.mutable.ArrayBuffer.empty[N_call]
	}
	// TBD: AAActivated etc to inherit from 1 trait; params: 1 node, many children
	// adjust insert method
	// CommunicationMatching should have Set[CommunicationRelation] (?), and have List[Communicators]
	// timestamp of Communicators should be determined by timestamp of newest N_call node
	//
	//
	// Prolog/Linda style question: 
	// can we test all possible communications of a certain type?
	// i.e. 
	// ->..?p:Int? loops and matches all sent integers like <-*1  <-*3
	
	case class InvokeFromET(node: CallGraphNodeTrait, payload: () => Unit) extends CallGraphMessageN {type N = CallGraphNodeTrait; def priority = PRIORITY_InvokeFromET}
	
