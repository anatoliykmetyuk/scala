// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import subscript.vm.model.template.TemplateNode

case class N_annotation[CN<:CallGraphNode.Child, CT<:TemplateNode.Child](template: T_annotation[CN,CT])
    extends CallGraphTreeNode
{
  type T = T_annotation[CN,CT]
  def there: CN = children.head.asInstanceOf[CN]
}

case class N_1_ary_op(template: T_1_ary_op) extends CallGraphTreeNode {
  type T = T_1_ary_op
  var continuation: Continuation1 = null
}

// Conditional Boolean operators
case class N_if(template: T_if)
  extends CallGraphTreeNode 
  {type T = T_if}

case class N_if_else(template: T_if_else) 
  extends CallGraphTreeNode 
  {type T = T_if_else}


// Conditional script operators
case class N_do_then(template: T_do_then) 
  extends CallGraphTreeNode 
  {type T = T_do_then}

case class N_do_else(template: T_do_else) 
  extends CallGraphTreeNode 
  {type T = T_do_else}

case class N_do_then_else(template: T_do_then_else) 
  extends CallGraphTreeNode 
  {type T = T_do_then_else}


/** N-ary */
case class N_n_ary_op(template: T_n_ary_op, isLeftMerge: Boolean)
    extends CallGraphTreeNode
    with    OptionalChildrenState
    with    VariablesContainer
{
  type T = T_n_ary_op   
  
  // Local state
  var isIteration                = false
  var hadFullBreak               = false
  var activationMode             = ActivationMode.Active
  var continuation: Continuation = null
  var lastActivatedChild: Child  = null

  
  // Helper and state methods
  def getLogicalKind = T_n_ary_op.getLogicalKind(template)
  def mustBreak = hadFullBreak = true
  

  // Overridden behavior
  override def addChild(c: Child) = {
    super.addChild(c); 
    if (isOptionalChild(c)) _nActivatedOptionalChildren += 1
    lastActivatedChild = c
  }
  
  override def childChangesSuccess(child: Child) = {
    val delta = if (child.hasSuccess) 1 else -1
    nActivatedChildrenWithSuccess += delta
    if (isOptionalChild(child)) nActivatedOptionalChildrenWithSuccess += delta
  }
  
  
  // Information
  override def infoString = extendedInfoString
  override def toString   = super.toString+(if(isIteration)" ..."else"")
}