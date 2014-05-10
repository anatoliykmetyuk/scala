package subscript.vm.executor.parts

import subscript.vm._

/**
 * Simple tracing and error tracking
 */
trait Tracer {
  var doTrace = false
  def trace(s: String) = if (doTrace) println(s)
  def error(s: String) {throw new Exception(s)}
  
  def traceAttribute(name: String, value: Any) = println(f"$name%41s: $value")
  def traceAttributes(n: N_n_ary_op, str: String) = {
    println(s"$str:")
    traceAttribute("activationMode", n.activationMode)
    traceAttribute("hadFullBreak", n.hadFullBreak)
    traceAttribute("nActivatedMandatoryChildren", n.nActivatedMandatoryChildren)
    traceAttribute("nActivatedMandatoryChildrenWithSuccess", n.nActivatedMandatoryChildrenWithSuccess)
    traceAttribute("nActivatedMandatoryChildrenWithoutSuccess", n.nActivatedMandatoryChildrenWithoutSuccess)
    traceAttribute("nActivatedOptionalChildren", n.nActivatedOptionalChildren)
    traceAttribute("nActivatedOptionalChildrenWithSuccess", n.nActivatedOptionalChildrenWithSuccess)
    traceAttribute("nActivatedOptionalChildrenWithoutSuccess", n.nActivatedOptionalChildrenWithoutSuccess)
    traceAttribute("indexChild_marksOptionalPart", n.indexChild_marksOptionalPart)
    traceAttribute("indexChild_marksPause", n.indexChild_marksPause)
    traceAttribute("aaActivated", n.aaActivated)
    traceAttribute("aaActivated_optional", n.aaActivated_optional)
  }  
}