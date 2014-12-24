package subscript.swing

import subscript.swing.Scripts._
import subscript.DSL._

object SubScriptDebugger extends SubScriptDebuggerApp
object SubScriptDebugger2 extends SubScriptDebuggerApp {
  // extra singleton to allow for GraphicalDebugging the SubScriptDebuggerApp
  override def doesThisAllowToBeDebugged = true
}

class SubScriptDebuggerApp extends SimpleSubscriptApplication with GraphicalDebugger {

  override def live: Unit = try _execute(_live, debugger=null, myScriptExecutor)
                            catch {case t:Throwable => t.printStackTrace; throw t}

  override def main(args: Array[String]) = super.main(args)

  def script..
     live       = (
                    {*awaitMessageBeingHandled(true)*}
                    ( if shouldStep then (
                        @{gui(there)}: {!updateDisplay!}
                        stepCommand || if autoCheckBox.selected then {*waitForStepTimeout*}
                    ) )
                    {messageBeingHandled(false)}
                    ... // TBD: parsing goes wrong without this comment; lineStartOffset was incremented unexpectedly
                  )
                  || exitDebugger

   stepCommand  = stepButton
   exitCommand  = exitButton
   exitDebugger = exitCommand @{gui(there)}:{exitConfirmed=confirmExit} while(!exitConfirmed)

}