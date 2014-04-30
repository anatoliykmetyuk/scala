package subscript.examples

import scala.language.postfixOps
 
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor._

import subscript.akka._
import subscript.Predef._
import subscript._
import subscript.DSL._
import subscript.vm._

 
object PingPong {
 
  class Pong extends SubScriptActor {

    def script..
      live = receivePing ...
      receivePing = r$({case msg @ "ping" => println(msg); sender ! "pong"})
  }
 
  class Ping(target: ActorRef) extends SubScriptActor {
 
    def script..
      live = times(3) sendPing receivePong
      sendPing = {target ! "ping"}
      receivePong = r$({case msg @ "pong" => println(msg)})
  }
 
 
  def main(args: Array[String]) {  
    val pong = SSARunnerV1Scheduler.system actorOf Props[Pong]
    val ping = SSARunnerV1Scheduler.system actorOf Props(classOf[Ping], pong)
    SSARunnerV1Scheduler.execute(null)
  }
}

