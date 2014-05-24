package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

trait OptionalChildrenState extends ChildrenState {this: CallGraphNode =>
  /*
   * some bookkeeping.
   * Optional children are children of this operator (which must be parallel or similar), 
   * which have been activated after an optional break had occurred, and for which no atomic action has happened.
   * This will in general happen in the "y" parts (and thereafter the "z" part) of
   * x & . & y
   * x & . & y & . & z
   */
  var _nActivatedOptionalChildren               = 0
  def nActivatedOptionalChildren                = _nActivatedOptionalChildren
  var nActivatedOptionalChildrenWithSuccess     = 0
  def nActivatedOptionalChildrenWithoutSuccess  = nActivatedOptionalChildren       - nActivatedOptionalChildrenWithSuccess
  def nActivatedMandatoryChildren               = nActivatedChildren               - nActivatedOptionalChildren
  def nActivatedMandatoryChildrenWithSuccess    = nActivatedChildrenWithSuccess    - nActivatedOptionalChildrenWithSuccess
  def nActivatedMandatoryChildrenWithoutSuccess = nActivatedChildrenWithoutSuccess - nActivatedOptionalChildrenWithoutSuccess
  
  /*
   * Consider
   *   x & . & y        and
   *   x & . & y . & z
   * After activation of x, the activation is paused at the first period (".") in case atomic actions had been activated in x.
   * The pause is then undone, and y is activated as an optional part.
   * The position in the call graph of the period between x and y is then identified by indexChild_marksOptionalPart
   * If after activation of y another period is encountered and atomic actions had been activated in y, 
   * then the position of that period is identified by indexChild_marksPause, and activation stops there.
   * If no atomic actions had been activated then activation just continues (maybe not a good strategy).
   * 
   * As soon as such an atomic action has happened, the z part is activated, and 
   * indexChild_marksOptionalPart becomes indexChild_marksPause, and the latter becomes -1.
   */
  var indexChild_marksOptionalPart = -1
  var indexChild_marksPause = -1
  var aaActivated = false
  var aaActivated_optional = false
  
  def resetNActivatedOptionalChildren = {
    _nActivatedOptionalChildren = 0
    nActivatedOptionalChildrenWithSuccess = 0
  }
  
  def isOptionalChild(c:Child) = {indexChild_marksOptionalPart >= 0 && c.index >= indexChild_marksOptionalPart}
}