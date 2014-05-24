package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

trait ChildrenState {this: CallGraphNode =>
  var nActivatedChildren = 0 // i.e. including the number of already deactivated children)
  var nActivatedChildrenWithSuccess = 0
  def nActivatedChildrenWithoutSuccess = nActivatedChildren - nActivatedChildrenWithSuccess
  def nActiveChildren = children.size
  def nDeactivatedChildren = nActivatedChildren - nActiveChildren
  
  def childChangesSuccess(child: Child) = {
    nActivatedChildrenWithSuccess += (if (child.hasSuccess) 1 else -1)
  }
  
  def extendedInfoString = f"$basicInfoString%.10s S=${hasSuccess} nActivated=${nActivatedChildren} (=${nActivatedChildrenWithSuccess}S+${nActivatedChildrenWithoutSuccess}N)"

  var aChildEndedInFailure = false
  def aChildEndedInSuccess = rightmostChildThatEndedInSuccess_index>=0
  def childThatEndedInSuccess_index(i: Int) = rightmostChildThatEndedInSuccess_index = 
             if (rightmostChildThatEndedInSuccess_index== -1) i else scala.math.max(rightmostChildThatEndedInSuccess_index, i)
                                  
  var rightmostChildThatEndedInSuccess_index = -1
  
}