package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import TemplateNode.Child
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._


// Local launches and calls
case class T_launch(
    override val child0: Child
) extends T_1_ary

case class T_launch_anchor(
    override val child0: Child
) extends T_1_ary

case class T_call(
             val calleeName: String,
    override val code      : N_call => Script[Unit]
) extends T_0_ary with TemplateCodeHolder[N_call, Script[Unit]]

// Root script types
case class T_script (
    override val owner : AnyRef,
    override val kind  : String,
                 name  : Symbol,
    override val child0: Child
) extends TemplateNode with TreeNode_1 with RootNode {
  override def toString = name.name
}

case class T_commscript(
    override val owner       : AnyRef,
    override val kind        : String,
                 communicator: Communicator
) extends TemplateNode with TreeNode_0 with RootNode {
  override def toString = super.toString+" "+communicator.name.name
}

case class T_communication(
    override val owner: AnyRef,
    override val kind : String,
                 names: Seq[Symbol]
) extends TemplateNode with TreeNode_0 with RootNode {
  override def toString = super.toString+" "+names.mkString(",")
}