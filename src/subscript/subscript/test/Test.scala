package subscript

import org.junit.runner.RunWith

import subscript.Predef._
import subscript.DSL._
import subscript.vm.{TemplateChildNode, N_code_unsure, CallGraphNodeTrait, UnsureExecutionResult, 
                     SimpleScriptDebugger}
import subscript.vm.executor._

class TestC {
    
  def script..
   times(n:Int) = while(here.pass < n)
   key(??c: Char) = {c='!'}
   main(args: Array[String]) = val c: Char = ' '
                               key(?c)  {println(s"key =>  $c")}
                               key('!') {println("key <= '!'")}

  val times1: Int=>Script[Unit] = n=>{_script(this, 'lambda) {_while{implicit here=>pass<n}}}
  val times2: Int=>Script[Unit] = n=> [ while(here.pass < n) ]
  //val times3: Int=>Script =     [ while(pass < _) ]
  //val noXml = [noXML]

  //TBD: allow ? in all parameter lists; parse < script > blocks
}
object Test extends TestC {
  // bridge method:
  def main( args: Array[String]): Unit = _execute(_main(args))
}