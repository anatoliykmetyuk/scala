package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._


case class T_code_normal(
    override val code: N_code_normal => Unit
) extends T_atomic_action[N_code_normal]

case class T_code_tiny(
    override val code: N_code_tiny => Unit
) extends T_atomic_action[N_code_tiny] 

case class T_code_threaded(
    override val code: N_code_threaded => Unit
) extends T_atomic_action[N_code_threaded]

case class T_code_unsure(
    override val code: N_code_unsure => Unit
) extends T_atomic_action[N_code_unsure]

case class T_code_eventhandling(
    override val code: N_code_eventhandling => Unit
) extends T_atomic_action[N_code_eventhandling]

case class T_code_eventhandling_loop(
    override val code: N_code_eventhandling_loop => Unit
) extends T_atomic_action[N_code_eventhandling_loop]