package subscript.swing

import java.util.concurrent.Executors

import scala.swing._
import scala.swing.event._

import scala.concurrent._
import scala.concurrent.duration._

object PureScalaDebugger extends PureScalaDebuggerApp

class PureScalaDebuggerApp extends SimpleSwingApplication with GraphicalDebugger {
  implicit val executionContext = ExecutionContext fromExecutorService Executors.newCachedThreadPool()

  def live = {

    def again: Unit = Future {awaitMessageBeingHandled(true)}.flatMap {_ =>
      if (shouldStep) {
        Swing.onEDTWait(updateDisplay)
        Future firstCompletedOf Seq(stepCommand, autoStepCommand)
      } else Future.successful(())
    }.onSuccess {case _ =>
      messageBeingHandled(false)
      again
    }

    again
    waitForExit
  }

  def stepCommand: Future[Unit] = {
    val promise = Promise[Unit]()
    new Reactor {
      stepButton.enabled = true
      listenTo(stepButton)
      reactions += {case _: ButtonClicked => promise.success(()); deafTo(stepButton)}
    }
    promise.future
  }

  def autoStepCommand: Future[Unit] = {
    lazy val never = Promise[Unit]().future
    if (autoCheckBox.selected) Future {waitForStepTimeout} else never
  }

  def waitForExit = {
    val liveAnchor = Promise[Unit]()
    new Reactor {
      exitButton.enabled = true
      listenTo(exitButton)
      reactions += {case _: ButtonClicked => liveAnchor.success(())}
    }
    Await.ready(liveAnchor.future, Duration.Inf)
  }

  override def main(args: Array[String]) = super.main(args)

}