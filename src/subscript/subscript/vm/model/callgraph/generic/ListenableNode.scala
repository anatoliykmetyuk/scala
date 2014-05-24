package subscript.vm.model.callgraph.generic

import subscript.vm.model.callgraph.NodeStateEvent

trait ListenableNode {this: Container =>
  import NodeStateEvent._
  def onActivate             : ()=>Unit  = getCodeProperty(OnActivate                  )
  def onActivateOrResume     : ()=>Unit  = getCodeProperty(OnActivateOrResume          )
  def onDeactivate           : ()=>Unit  = getCodeProperty(OnDeactivate                )
  def onDeactivateOrSuspend  : ()=>Unit  = getCodeProperty(OnDeactivateOrSuspend       )
  def onSuccess              : ()=>Unit  = getCodeProperty(OnSuccess                   )
  def onActivate           (c:   =>Unit) = setCodeProperty(OnActivate           , ()=>c)
  def onActivateOrResume   (c:   =>Unit) = setCodeProperty(OnActivateOrResume   , ()=>c)
  def onDeactivate         (c:   =>Unit) = setCodeProperty(OnDeactivate         , ()=>c)
  def onDeactivateOrSuspend(c:   =>Unit) = setCodeProperty(OnDeactivateOrSuspend, ()=>c)
  def onSuccess            (c:   =>Unit) = setCodeProperty(OnSuccess            , ()=>c)
}