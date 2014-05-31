// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import subscript.vm.model.template.TemplateNode

// Local launches and calls
case class N_launch(template: T_launch) 
  extends CallGraphLeafNode
  {type T = T_launch}

case class N_launch_anchor(template: T_launch_anchor)
  extends CallGraphTreeNode
  with    ChildrenState  // Make it extend this just because of extendedInfoString???
{
  type T = T_launch_anchor
  override def infoString = extendedInfoString
}

case class N_call[R](template: T_call[R]) extends CallGraphTreeNode {
  type T = T_call[R]
  var t_callee: T_script  = null
//var t_commcallee: T_commscript = null

//def communicator: Communicator = if (t_commcallee==null) null else t_commcallee.communicator
  def stopPending {}//if (communicator!=null) {communicator.removePendingCall(this)}} // stop pending unmatched communication partners
  var actualParameters: scala.collection.immutable.Seq[ActualParameter[_<:Any]] = Nil
  def calls(t: T_script, args: FormalParameter[_]*): Unit = {
    this.actualParameters = args.toList.map(_.asInstanceOf[ActualParameter[_]])
    this.t_callee = t
  }
  //def calls(t: T_commscript, args: FormalParameter[_]*): Unit = {
  //  this.actualParameters = args.toList.map(_.asInstanceOf[ActualParameter[_]])
  //  this.t_commcallee = t
  //}
  def allActualParametersMatch: Boolean = actualParameters.forall {_.matches}
  def transferParameters      : Unit    = actualParameters.foreach{_.transfer}
}


// Root script types
case class Script[R](var template: T_script, p: FormalParameter[_]*)
  extends CallGraphTreeNode with ScriptResultHolder[R]
  {type T = T_script}

//case class N_communication(var template: T_communication) extends CallGraphNode {
//  type T = T_communication
//  
//  var communication: Communication = null
//  
//  def inits(t: T_communication, owner: Any): TemplateNode = null // TBD
//  def _getParameter[T](p: Symbol): CommunicationParameter[T] = CommunicationParameter[T](p)
//}