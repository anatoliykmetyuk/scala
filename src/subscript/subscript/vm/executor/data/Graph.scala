package subscript.vm.executor.data

import subscript.vm.executor._
import subscript.vm.N_code_eventhandling
import subscript.vm.N_while
import subscript.vm.T_then_else
import subscript.vm.T_code_normal
import subscript.vm.N_code_threaded
import subscript.vm.T_launch_anchor
import subscript.vm.T_if
import subscript.vm.N_n_ary_op
import subscript.vm.N_break
import subscript.vm.T_localvar
import subscript.vm.N_loop
import subscript.vm.N_then
import subscript.vm.N_annotation
import subscript.vm.N_optional_break_loop
import subscript.vm.N_localvar
import subscript.vm.T_break
import subscript.vm.N_if
import subscript.vm.T_nu
import subscript.vm.N_code_unsure
import subscript.vm.N_epsilon
import subscript.vm.T_1_ary_op
import subscript.vm.T_launch
import subscript.vm.T_annotation
import subscript.vm.T_script
import subscript.vm.N_launch_anchor
import subscript.vm.N_launch
import subscript.vm.N_script
import subscript.vm.T_optional_break
import subscript.vm.N_nu
import subscript.vm.T_optional_break_loop
import subscript.vm.T_loop
import subscript.vm.T_code_eventhandling_loop
import subscript.vm.T_code_tiny
import subscript.vm.CallGraphParentNodeTrait
import subscript.vm.N_1_ary_op
import subscript.vm.CallGraphTreeNode
import subscript.vm.T_code_eventhandling
import subscript.vm.T_n_ary_op
import subscript.vm.CallGraphNodeTrait
import subscript.vm.T_code_unsure
import subscript.vm.N_delta
import subscript.vm.N_code_tiny
import subscript.vm.T_while
import subscript.vm.N_optional_break
import subscript.vm.T_delta
import subscript.vm.N_code_eventhandling_loop
import subscript.vm.N_call
import subscript.vm.N_privatevar
import subscript.vm.N_then_else
import subscript.vm.T_then
import subscript.vm.T_if_else
import subscript.vm.N_if_else
import subscript.vm.T_call
import subscript.vm.T_epsilon
import subscript.vm.T_privatevar
import subscript.vm.N_code_normal
import subscript.vm.T_code_threaded
import subscript.vm.TemplateNode
import subscript.vm.LocalVariable
import subscript.vm.Activation

class Graph(val executor: ScriptExecutor) {
  import Graph._
  private   val anchorTemplate =     T_call("<root>", null)
  private   val rootTemplate   = new T_launch_anchor(anchorTemplate) {override def owner = executor}
  
  val rootNode       =     N_launch_anchor(rootTemplate)
  val anchorNode     =     N_call(anchorTemplate)
 
  connect(parentNode = rootNode, childNode = anchorNode)
  
  private var nNodes = 0
  def nextNodeIndex = {nNodes = nNodes+1; nNodes}
  
  // Graph
  def activateFrom(parent: CallGraphParentNodeTrait, template: TemplateNode, pass: Option[Int] = None): CallGraphTreeNode = {
    import Graph._
    val n = createNode(template, executor)
    n.pass = pass.getOrElse(if(parent.isInstanceOf[N_n_ary_op]) 0 else parent.pass)
    connect(parentNode = parent, childNode = n)
    // ?? probably delete the following line
    //n match {case ns: N_script => val pc = ns.parent.asInstanceOf[N_call]; what to do with this}
    executor.msgQueue insert Activation(n)
    n
  }
}

object Graph {
  def connect(parentNode: CallGraphParentNodeTrait, childNode: CallGraphTreeNode) {
    childNode.parent = parentNode
    childNode.scriptExecutor = parentNode.scriptExecutor
    parentNode.appendChild(childNode)
    parentNode.nActivatedChildren += 1
  }  
  
  def disconnect(childNode: CallGraphNodeTrait) {
    childNode match {
      case cn: CallGraphTreeNode => val parentNode = cn.parent
                                    if (parentNode==null) return;
                                    parentNode.children -= cn
       case _ =>
    }
  }
  
  def setIteration_n_ary_op_ancestor(n: CallGraphNodeTrait) = {
    val a = n.n_ary_op_ancestor
    if (a!=null) a.isIteration = true
  }
  
  def createNode(template: TemplateNode, scriptExecutor: ScriptExecutor): CallGraphTreeNode = {
   val result =
    template match {
      case t @ T_optional_break         (                          ) => N_optional_break(t)
      case t @ T_optional_break_loop    (                          ) => N_optional_break_loop(t)
      case t @ T_loop                   (                          ) => N_loop          (t)
      case t @ T_break                  (                          ) => N_break         (t)
      case t @ T_delta                  (                          ) => N_delta         (t)
      case t @ T_epsilon                (                          ) => N_epsilon       (t)
      case t @ T_nu                     (                          ) => N_nu            (t)
      case t @ T_call                   (        _,    _           ) => N_call          (t)
      case t @ T_privatevar             (              name        ) => N_privatevar    (t)
      case t @ T_localvar               (_,_,lv:LocalVariable[_],_ ) => N_localvar      (t)
      case t @ T_code_normal            (              _           ) => N_code_normal   (t)
      case t @ T_code_unsure            (              _           ) => N_code_unsure   (t)
      case t @ T_code_tiny              (              _           ) => N_code_tiny     (t)
      case t @ T_code_threaded          (              _           ) => N_code_threaded (t)
      case t @ T_code_eventhandling     (              _           ) => N_code_eventhandling     (t)
      case t @ T_code_eventhandling_loop(              _           ) => N_code_eventhandling_loop(t)
      case t @ T_while                  (              _           ) => N_while         (t)
      case t @ T_launch                 (              _           ) => N_launch        (t)
      case t @ T_launch_anchor          (              _           ) => N_launch_anchor (t)
      case t @ T_1_ary_op               (kind: String, _           ) => N_1_ary_op      (t)
      case t @ T_annotation             (              _, _        ) => N_annotation    (t)
      case t @ T_if                     (              _, _        ) => N_if            (t)
      case t @ T_if_else                (              _, _, _     ) => N_if_else       (t)
      case t @ T_then                   (              _, _        ) => N_then          (t)
      case t @ T_then_else              (              _, _, _     ) => N_then_else     (t)
      case t @ T_n_ary_op               (kind: String, children@ _*) => N_n_ary_op (t, T_n_ary_op.isLeftMerge(kind))
      case t @ T_script(_, kind: String, name: Symbol, child0: TemplateNode) => N_script(t)
      case _ => null 
    }
    result.codeExecutor = CodeExecutor.defaultCodeFragmentExecutorFor(result, scriptExecutor)
    result
  }
}