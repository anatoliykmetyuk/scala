package subscript.vm.executor

import subscript.vm._

object CodeExecutor {
  def defaultCodeFragmentExecutorFor(node: CallGraphNodeTrait, scriptExecutor: ScriptExecutor): CodeExecutorTrait = {
    node match {
      case n@N_code_normal  (_) => new   NormalCodeFragmentExecutor(n, scriptExecutor)
      case n@N_code_unsure  (_) => new   UnsureCodeFragmentExecutor(n, scriptExecutor)
      case n@N_code_threaded(_) => new ThreadedCodeFragmentExecutor(n, scriptExecutor)
      case _                    => new          TinyCodeExecutor(node, scriptExecutor)
    }
  }
  
  def executeCode[R](n: DoCodeHolder[R]) : R                  = executeCode(n, ()=>n.doCode)
  def executeCode[R](n: DoCodeHolder[R], code: =>()=>R   ): R = {n.codeExecutor.doCodeExecution(code)}
  def executeCodeIfDefined(n: CallGraphNodeTrait, code: =>()=>Unit): Unit = {if (code!=null) executeCode(n.asInstanceOf[DoCodeHolder[Unit]], code)} // TBD: get rid of cast
  
}