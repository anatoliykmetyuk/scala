package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._


case class T_localvar[V](
                 isVal        : Boolean,
                 isLoop       : Boolean,
                 localVariable: LocalVariable[V],
    override val code         : N_localvar[V] => V
) extends T_0_ary with TemplateCodeHolder[N_localvar[V],V]

case class T_privatevar(name: Symbol) extends T_0_ary

case class T_local_valueCode[V<:Any] (
    override val kind         : String,
                 localVariable: LocalVariable[V],
    override val code         : N_localvar[V]=>V
) extends T_0_ary with TemplateCodeHolder[N_localvar[V], V]
