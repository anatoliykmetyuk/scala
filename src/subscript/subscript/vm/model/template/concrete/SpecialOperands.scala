package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._


// Loops
case class T_while(override val code: N_while => Boolean) extends T_0_ary with TemplateCodeHolder[N_while, Boolean]
case class T_loop() extends T_0_ary

// Breaks
case class T_break()                extends T_0_ary
case class T_optional_break()       extends T_0_ary
case class T_optional_break_loop()  extends T_0_ary

// Constants
case class T_delta()   extends T_0_ary
case class T_epsilon() extends T_0_ary
case class T_nu()      extends T_0_ary