package subscript.vm.executor.parts

import subscript.vm._
import subscript.vm.executor._
import subscript.vm.executor.data._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import scala.collection.mutable.Buffer
import subscript.vm.model.callgraph._

import CodeExecutor._
import CallGraph._
import MessageHandlers._

trait ContinuationHandler {this: ScriptExecutor[_] with Tracer =>
  import msgQueue._
  import graph.{activateFrom, linkNode}
  
  val continuationHandler: MessageHandler = {
    case a@Continuation(_) => handleContinuation(a)
  }

  /*
   * handleContinuation:
   * 
   * The most complicated method of the Script Executor: 
   * determine what an N-ary operator will do
   * after it has received a set of messages. Potential outcomes include:
   * - activate next operand and/or have success
   * - suspend, resume, exclude
   * - deactivate
   * - nothing
   *   
   * The decision is based on three aspects:
   * - the kind of operator
   * - the state of the node
   * - the received messages
  */ 
  def handleContinuation(message: Continuation): Unit = new Decisions(message) {
    decideProgress
    decideExclusion
    decideSuccess
    
    if (doTrace) trace
    
    executeDecisions
  }
  
  trait Stateful extends ProgressState with ExclusionState {
    val message: Continuation
    val node = message.node.asInstanceOf[N_n_ary_op]
    
    val isSequential = node.template.kind match {
      case ";" | "|;"  | "||;" |  "|;|" => true
      case _ => false
    }
    
    var childNode: CallGraphNode = null // may indicate node from which the a message came
  }
  
  trait ProgressState {this: Stateful =>
    var activateNext              = false
    var activationEnded           = false
    var activationEndedOptionally = false

    var nextActivationTemplateIndex = 0
    var nextActivationPass = 0
    
    var shouldSucceed = false    
  }
  
  trait ExclusionState {this: Stateful =>
    var nodesToBeExcluded : Seq[CallGraphNode.Child] = null
    var nodesToBeSuspended: Seq[CallGraphNode.Child] = null    
  }
    

  
  trait ProgressDecisions extends Stateful {
    private var activateNextOrEnded = false
    
    def decideProgress {
      if (isSequential       ) sequential else parallel
      if (activateNextOrEnded) clarifyProgress
    }
    
    private def sequential {
      val s = message.success
      val b = message.break
      
      if (b!=null) {
        activateNextOrEnded = true
        childNode = b.child
        if (b.activationMode==ActivationMode.Optional)
          activationEndedOptionally = true
        else node.mustBreak
      }
      
      if (s!=null) {
        activateNextOrEnded = true
        childNode = s.child
      }
    }
    
    private def parallel: Unit =
      if (node.activationMode!=ActivationMode.Inactive)
        node.template.kind match {
//          case "%" => parallelPercent 
        
          case "+" | "|+" | "|+|"
          => parallelPlus
          
          case kind if
            T_n_ary_op.isLeftMerge(kind)
          => parallelLeftMerge
          
          case "/" | "|/"  | "|/|" if
            node.indexChild_marksPause >= 0 &&
            message.aaHappeneds!=Nil
          => parallelInterrupt
          
          case _ if
            node.indexChild_marksPause >= 0 &&
            message.aaHappeneds!=Nil        &&
            message.aaHappeneds.exists
              {a=>a.child.index > node.indexChild_marksOptionalPart}
          => parallelBreakPause
          
          case _ if
            node.indexChild_marksOptionalPart >= 0 &&
            message.aaHappeneds!=Nil               &&
            message.aaHappeneds.exists
             {a=>a.child.index > node.indexChild_marksOptionalPart}
          => parallelBreakOptional
          
          case _
          => parallelOther
      }
    
    private def clarifyProgress  {
      // old: childNode = if (T_n_ary.isLeftMerge(node.template.kind)) node.lastActivatedChild else message.childNode ; now done before
      nextActivationTemplateIndex = childNode.template.indexAsChild+1
      nextActivationPass = childNode.pass
      
      message.node.activationMode = ActivationMode.Active
      
      def ackNext = !(activationEnded || activationEndedOptionally)
      if (node.hadFullBreak) activationEnded = true
      else if (nextActivationTemplateIndex==message.node.template.children.size) {
        if (message.node.isIteration) {
          nextActivationTemplateIndex = 0
          nextActivationPass += 1
          activateNext = ackNext
        }
        else activationEnded = true
      }
      else activateNext = ackNext
    }
    
    /**
     * This is commented out.
     */
    private def parallelPercent {
//      case "%" => val d = message.deactivations; 
//                  val b = message.break
//                  if (d!=Nil) {
//                    activateNextOrEnded = d.size>0 && !d.head.excluded
//                    if (activateNextOrEnded) {
//                      childNode = d.head.node
//                    }
//                  }
    }
    
    /**
     * "+" node activations may be broken by "." if no atomic actions had been activated
     */
    private def parallelPlus {
      val a = message.aaActivated
      val c = message.caActivated
      val b = message.break
      
      activateNextOrEnded = if (b==null) true
                       else if (b.activationMode==ActivationMode.Optional) a!=null || c!=null
                       else false 
      
      if (activateNextOrEnded) childNode = node.lastActivatedChild         
    }
    
    private def parallelLeftMerge {
       val aa = message.aaActivated
       val ca = message.caActivated
       val as = message.aaHappeneds
       val b  = message.break
       activateNextOrEnded = aa==null && ca==null ||
                             as!=Nil  && as.exists( (as:AAHappened) => as.node==node.lastActivatedChild )
       if (b!=null) {
         // ???
       }
       if (activateNextOrEnded) childNode = node.lastActivatedChild
    }
    
    /**
     *       . / a     => no pause; a is optional
     * (-) / . / a     => no pause; a is optional
     *  a  / . / b     => pause; after a happens / is done
     */
    private def parallelInterrupt =
      if (
        message.aaHappeneds.exists
          {_.node.index > node.indexChild_marksOptionalPart} &&
        message.deactivations==Nil
      ) switchOptionalPart(false)
    
    /**
     *       . & a     => no pause; a is optional
     * (+) & . & a     => no pause; a is optional
     *  a  & . & b     => pause; after a happens b is activated as optional
     * 
     */
    private def parallelBreakPause {
      if (doTrace) traceAttributes(node, "A")
      switchOptionalPart(false)
    }
    
    private def parallelBreakOptional {
      if (doTrace) traceAttributes(node, "B")
      switchOptionalPart(false)
    }
    
    private def parallelOther {
       val b = message.break
       if (b==null) {
         if (message.aaHappeneds != Nil || message.activation != null) {
           if (doTrace) traceAttributes(node, "C")
           activateNextOrEnded = true
           childNode = node.lastActivatedChild
         }
         else if (doTrace) traceAttributes(node, "D")
       }
       else if (b.activationMode==ActivationMode.Optional) {
         if (node.aaActivated_optional) {
           if (doTrace) traceAttributes(node, "E")
           switchOptionalPart(false)
         }
         else if (message.aaActivated != null
              ||  node.indexChild_marksOptionalPart < 0 && node.aaActivated){ // TBD: possibly wrong test; 
                                        // we should ask whether any AA had been activated in the part before "."
           if (doTrace) traceAttributes(node, "F")
           node.activationMode = ActivationMode.Optional // TBD: may possibly be dropped???; check node.indexChild_marksPause >= 0
           node.indexChild_marksPause = b.child.index
         }
         else {
           if (doTrace) traceAttributes(node, "G")
           switchOptionalPart(true)
         }
       }
    }
    
    // TBD: come up with more meaningful names for the
    // method and its argument
    private def switchOptionalPart(toOptional: Boolean = false) {
      activateNextOrEnded = true
      
      if (!toOptional) {
        node.activationMode = ActivationMode.Active
        node.resetNActivatedOptionalChildren
        node.indexChild_marksOptionalPart = node.indexChild_marksPause
        node.aaActivated_optional         = false
      } else {
        node.activationMode = ActivationMode.Optional
        node.resetNActivatedOptionalChildren
        node.indexChild_marksOptionalPart = message.break.child.index
      }
      
      node.indexChild_marksPause        = -1
      childNode = node.lastActivatedChild
    }
  }
  trait ExclusionDecisions extends Stateful {
    
    def decideExclusion = node.template.kind match {
      case "/"  | "|/"  | "|/|"        => interrupt
      case "&&" | "&&:" | "||" | "||:" => parallel
      case _ =>
    }

    private def interrupt {
      // deactivate to the right when one has finished successfully
      // TBD: something goes wrong here; LookupFrame2 does not "recover" from a Cancel search
      message.deactivations match {
        case d::tail if d.child.hasSuccess && !d.excluded =>
          nodesToBeExcluded = node.children.filter(_.index>d.child.index)
        
        case _ =>
      }
    }
    
    private def parallel {
      val isLogicalOr = T_n_ary_op.getLogicalKind(node.template.kind)==LogicalKind.Or
      /* 
       * Result decisive nodes are the nodes that decide
       * the result of the whole operator.
       * For example, one single node that ended in success decides
       * the result of an Or-parallel operator, and one single node that
       * ended with failure decides the result of an And-parallel operator.
       */
      val resultDecisiveNodes = message.deactivations.
        map(_.child).
        filter(_.hasSuccess==isLogicalOr)
        
      if (!resultDecisiveNodes.isEmpty) {
        nodesToBeExcluded = node.children diff resultDecisiveNodes
        activateNext = false
      }
    }
  }
  trait SuccessDecisions extends Stateful {
    
    def decideSuccess = if (!shouldSucceed && !node.hasSuccess)
      node.template.kind match {
        case ";" => sequential
        case "/" => interrupt
        
        case _   => T_n_ary_op.getLogicalKind(node.template.kind) match {
          case LogicalKind.None =>
          case LogicalKind.And  => logicalAnd
          case LogicalKind.Or   => logicalOr
        }
      }
      
    private def sequential = shouldSucceed = activationEnded || activationEndedOptionally
    
    private def interrupt  = shouldSucceed = message.success != null ||
                                     message.aaHappeneds.exists(_.child.index<node.rightmostChildThatEndedInSuccess_index) || 
                                     node.nActivatedChildrenWithSuccess > 0
        
    private def logicalAnd  = shouldSucceed = !activateNext &&
                      node.nActivatedMandatoryChildrenWithoutSuccess == 0
   
    private def logicalOr   = shouldSucceed = node.nActivatedChildrenWithSuccess > 0
  }
  trait DecisionsExecution extends Stateful {
    def executeDecisions {
      succeed
      exclude
      activate
    }
    
    private def succeed = if (shouldSucceed) insert(Success(node))   // TBD: prevent multiple successes at same "time"
    
    private def exclude = {
      if (nodesToBeExcluded !=null) nodesToBeExcluded .foreach(n => insert(Exclude(node, n)))
      if (nodesToBeSuspended!=null) nodesToBeSuspended.foreach(n => insert(Suspend(      n)))
    }
    
    private def activate {
      if (isSequential) activateNext = activateNext && node.children.isEmpty
      if (activateNext) {
        val t = message.node.template.children(nextActivationTemplateIndex)
        activateFrom(message.node, t, Some(nextActivationPass))
        val activation = if (message.activation != null) message.activation else Activation(message.node)
        
        val nary_op_isLeftMerge = node match {
          case N_n_ary_op (t: T_n_ary, isLeftMerge) => isLeftMerge
          case _                                    => false
        }
        if (!nary_op_isLeftMerge) insertContinuation(activation, node)
      }
      else if (node.children.isEmpty) insertDeactivation(node, null)
    }
  }

  
  class Decisions(val message: Continuation) extends
      ProgressDecisions  with
      ExclusionDecisions with
      SuccessDecisions   with
      DecisionsExecution {   
    node.continuation = null
    
    def trace {
      traceAttributes(node, "Finally")
      traceAttribute("activateNext", activateNext)
      traceAttribute("activationEnded", activationEnded)
      traceAttribute("activationEndedOptionally", activationEndedOptionally)
      traceAttribute("shouldSucceed", shouldSucceed)
    }
  }
}