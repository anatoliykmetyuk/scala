package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import TemplateNode.Child
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._


// Local launches and calls
case class T_launch(
    override var child0: Child
) extends T_1_ary

case class T_launch_anchor(
    override var child0: Child
) extends T_1_ary

case class T_call[R](
             val calleeName: String, // TBD: make symbol ?
    override val code      : N_call[R] => Script[R]
) extends T_0_ary with TemplateCodeHolder[Script[R],N_call[R]]

// Root script types
case class T_script (
    override val owner : AnyRef,
    override val kind  : String,
                 name  : Symbol,
    override var child0: Child
) extends TemplateNode with TreeNode_1 with RootNode {
  override def toString = name.name
  def setChild(c: Child) = child0 = c
}

//case class T_commscript(
//    override val owner       : AnyRef,
//    override val kind        : String,
//                 communicator: Communicator
//) extends TemplateNode with TreeNode_0 with RootNode {
//  override def toString = super.toString+" "+communicator.name.name
//}

//case class T_communication(
//    override val owner: AnyRef,
//    override val kind : String,
//                 names: Seq[Symbol]
//) extends TemplateNode with TreeNode_0 with RootNode {
//  override def toString = super.toString+" "+names.mkString(",")
//}