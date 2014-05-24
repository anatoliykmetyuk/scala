package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

trait ExecutionResult {this: CallGraphNode =>
  private var _result = ExecutionResult.Success; // TBD: clean this all up; hasSuccess+result is too much
  def result = _result
  def result_=(value: ExecutionResult.ExecutionResultType): Unit = {
    _result = value
    hasSuccess = value == ExecutionResult.Success
  }
  def fail   = result = ExecutionResult.Failure
  def ignore = result = ExecutionResult.Ignore
}

trait LoopExecutionResult extends ExecutionResult {this: CallGraphNode =>
  def break          = result = ExecutionResult.Break
  def break_optional = result = ExecutionResult.OptionalBreak
}