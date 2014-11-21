import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._
import subscript.vm.executor._

// Subscript sample application: Hello+(+); World
//
object Hello_1_World {
  def main(args: Array[String]) {_execute(_main())}
   
  def script main = {println("Hello,")} + (+); 
                    {println("world!")}
}

