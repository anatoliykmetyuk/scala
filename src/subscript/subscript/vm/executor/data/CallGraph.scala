package subscript.vm.executor.data

import subscript.vm.executor._
import subscript.vm._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.vm.model.callgraph.CallGraphNode
import subscript.vm.model.callgraph.CallGraphTreeNode

class CallGraph[S](val executor: ScriptExecutor[S]) {
  import CallGraph._
  private   val anchorTemplate =     T_call[S]("<root>", null)
  private   val rootTemplate   = new T_launch_anchor(anchorTemplate) {override def owner = executor}
  
  val rootNode       =     N_launch_anchor(rootTemplate)
  val anchorNode     =     N_call[S](anchorTemplate)
 
  /**
   * Must be called before this graph is used.
   */
  def init() {
    rootNode.scriptExecutor = executor
    connect(parentNode = rootNode, childNode = anchorNode)
  }
  
  private var nNodes = 0
  def nextNodeIndex = {nNodes = nNodes+1; nNodes}
  
  // Graph
  def activateFrom(parent: CallGraphNode.Parent, template: TemplateNode, pass: Option[Int] = None): CallGraphNode = {
    import CallGraph._
    val n = createNode(template, executor)
    n.pass = pass.getOrElse(if(parent.isInstanceOf[N_n_ary_op]) 0 else parent.pass)
    connect(parentNode = parent, childNode = n)
    // ?? probably delete the following line
    //n match {case ns: N_script => val pc = ns.parent.asInstanceOf[N_call]; what to do with this}
    executor.msgQueue insert Activation(n)
    n
  }
}

object CallGraph {
  def connect(parentNode: CallGraphNode.Parent, childNode: CallGraphNode.Child) {
    childNode addParent parentNode
    childNode.scriptExecutor = parentNode.scriptExecutor
    parentNode.nActivatedChildren += 1
  }  
  
  def disconnect(childNode: CallGraphNode.Child) {
    childNode match {
      case cn: CallGraphTreeNode => cn removeParent cn.parent
      case _ =>
    }
  }
  
  def setIteration_n_ary_op_ancestor(n: CallGraphNode) = {
    val a = n.n_ary_op_ancestor
    if (a!=null) a.isIteration = true
  }
  
  def createNode(template: TemplateNode, scriptExecutor: ScriptExecutor[_]): CallGraphNode = {
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
      case t @ T_do_then                (              _, _        ) => N_do_then       (t)
      case t @ T_do_else                (              _, _        ) => N_do_else       (t)
      case t @ T_do_then_else           (              _, _, _     ) => N_do_then_else  (t)
      case t @ T_n_ary_op               (kind: String, children@ _*) => N_n_ary_op (t, T_n_ary_op.isLeftMerge(kind))
    //case t @ T_script(_, kind: String, name: Symbol, child0: TemplateNode) => N_script(t) node should be created by the script-method call
      case _ => null 
    }
    result.codeExecutor = CodeExecutor.defaultCodeFragmentExecutorFor(result, scriptExecutor)
    result
  }
}