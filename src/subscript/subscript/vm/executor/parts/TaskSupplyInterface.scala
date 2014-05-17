package subscript.vm.executor.parts

import subscript.vm._
import subscript.vm.executor._
import subscript.vm.executor.data._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._

import subscript.DSL._

trait TaskSupplyInterface {this: ScriptExecutor =>
  def invokeFromET(f: => Unit) = msgQueue sInsert InvokeFromET(graph.rootNode, () => f)
  
  def launch(n: CallGraphNodeTrait, aScript: Script[_])  {
    val launchAnchor       = CallGraphNode.getLowestLaunchAnchorAncestor(n) // could be rootNode
    val callAnchorTemplate =     T_call("<launched>", null)
    val callAnchorNode     =     N_call(callAnchorTemplate)
    Graph.connect(parentNode = launchAnchor, childNode = callAnchorNode)
    // callAnchorTemplate.parent = launchAnchor.template // would not be mutual...
    aScript(callAnchorNode)
    graph.activateFrom(callAnchorNode, callAnchorNode.t_callee, Some(0))
  }
  
}