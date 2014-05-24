// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._

case class N_code_normal   (template: T_code_normal  )
  extends N_atomic_action[N_code_normal  ]
  {type T = T_code_normal}

case class N_code_tiny     (template: T_code_tiny    )
  extends N_atomic_action[N_code_tiny    ]
  {type T = T_code_tiny}

case class N_code_threaded (template: T_code_threaded)
  extends N_atomic_action[N_code_threaded]
  {type T = T_code_threaded}

case class N_code_unsure   (template: T_code_unsure  )
  extends N_atomic_action[N_code_unsure  ]
  with    ExecutionResult
  {type T = T_code_unsure  }

case class N_code_eventhandling     (template: T_code_eventhandling     )
  extends N_atomic_action[N_code_eventhandling]
  {type T = T_code_eventhandling}

case class N_code_eventhandling_loop(template: T_code_eventhandling_loop)
  extends N_atomic_action[N_code_eventhandling_loop]
  with    LoopExecutionResult
  {type T = T_code_eventhandling_loop}