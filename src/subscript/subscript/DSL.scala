/*,C],R]
    This file is part of Subscript - an extension of the Scala language 
                                     with constructs from Process Algebra.

    Subscript is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License and the 
    GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Subscript consists partly of a "virtual machine". This is a library; 
    Subscript applications may distribute this library under the 
    GNU Lesser General Public License, rather than under the 
    GNU General Public License. This way your applications need not 
    be made Open Source software, in case you don't want to.

    Subscript is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You may have received a copy of the GNU General Public License
    and the GNU Lesser General Public License along with Subscript.
    If not, see <http://www.gnu.org/licenses/>
*/

package subscript

import scala.language.implicitConversions
import scala.collection.mutable.LinkedList

// VM
import subscript.vm._
import subscript.vm.executor._

// Template
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import TemplateNode.Child

// Call graph
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._

/*
 * Internal Scala DSL for SubScript.
 * Using this DSL one can make SubScript programs without the need 
 * for a compiler that understands the specific SubScript syntax. 
 * 
 * Also this DSL may well be the target for a SubScript extension to the Scala compiler.
 * 
 * Usage: see example programs
 */
object DSL {
  def _script[S](owner:AnyRef, name:Symbol, p: FormalParameter[_]*)(childTemplateAt: (Script[S])=>TemplateNode.Child): ScriptNode[S] = {
    // In order to create the Script, we need to know T_script, the tempalte
    // To create the template, we should not need to know its children
    // because to create the children, we need to know Script first
    
    val template = T_script(owner, "script", name)
    val result: ScriptNode[S] = new ScriptNode(template, p:_*)
    template.setChild(childTemplateAt(result))
    result
  }
  //def _comscript(owner : AnyRef, communicator: Communicator, p: FormalParameter[_]*)                       : Script[Unit] = {(_c: N_call) => _c.calls(T_commscript(owner, "communicator" , communicator), p:_*)}
  
// TBD: communication scripts
//  def _communication(owner: Any, names: Symbol*): N_communication => TemplateNode = {
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }
//  def _communication(owner: Any, names: Symbol*)(_body: N_communication => TemplateNode) = { 
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }

  def getScriptTemplate    [S](s: ScriptNode[S]): T_script     = s.template // TBD: check; was: {val nc = N_call(T_call("", null)); s(nc); nc.t_callee}
  def getScriptBodyTemplate[S](s: ScriptNode[S]): TemplateNode = getScriptTemplate(s).child0
  def toScriptString       [S](s: ScriptNode[S]): String       = getScriptTemplate(s).hierarchyString
  def toScriptBodyString   [S](s: ScriptNode[S]): String       = {val c = getScriptBodyTemplate(s); if(c==null) "" else c.hierarchyString}
//def _communication(body: N_communication => TemplateNode) = Communication(body)
//def _communicator(name: Symbol) = Communicator(name)
//def _relate(communication: Communication, crs: CommunicatorRole*): Unit = communication.setCommunicatorRoles(crs.toList)

//implicit def communicatorToCommunicatorRole(c: Communicator) = new CommunicatorRole(c)
  
  def _execute[S     ](_script: ScriptNode[S]                             ): ScriptExecutor[S] = _execute(_script, null, true)
  def _execute[S<:X,X](_script: ScriptNode[S], executor: ScriptExecutor[X]): ScriptExecutor[X] = _execute(_script, null, executor)
  def _execute[S     ](_script: ScriptNode[S], debugger: MsgListener      ): ScriptExecutor[S] = _execute(_script, debugger, false)
  def _execute[S     ](_script: ScriptNode[S], allowDebugger: Boolean     ): ScriptExecutor[S] = _execute(_script, null, allowDebugger)
  def _execute[S     ](_script: ScriptNode[S], debugger: MsgListener
                                         , allowDebugger: Boolean     ): ScriptExecutor[S] = {
    val executor = ScriptExecutorFactory.createScriptExecutor[S](allowDebugger && debugger == null)
    _execute(_script, debugger, executor)
  }
  def _execute[S<:X,X](_script: ScriptNode[S], debugger: MsgListener, executor: ScriptExecutor[X]): ScriptExecutor[X] = {
    if (debugger!=null) debugger.attach(executor)
    
    try {
      executor.run(_script)
    }
    catch {case t:Throwable => println(s"DSL._execute caught throwable: $t"); throw t}
  }

  def _normal             [R](cf: N_code_normal            [R] =>R, mustPropagateResultValue: Boolean) = T_code_normal            (cf, mustPropagateResultValue)
  def _threaded           [R](cf: N_code_threaded          [R] =>R, mustPropagateResultValue: Boolean) = T_code_threaded          (cf, mustPropagateResultValue)
  def _unsure             [R](cf: N_code_unsure            [R] =>R, mustPropagateResultValue: Boolean) = T_code_unsure            (cf, mustPropagateResultValue)
  def _tiny               [R](cf: N_code_tiny              [R] =>R, mustPropagateResultValue: Boolean) = T_code_tiny              (cf, mustPropagateResultValue)
  def _eventhandling      [R](cf: N_code_eventhandling     [R] =>R, mustPropagateResultValue: Boolean) = T_code_eventhandling     (cf, mustPropagateResultValue)
  def _eventhandling_loop [R](cf: N_code_eventhandling_loop[R] =>R, mustPropagateResultValue: Boolean) = T_code_eventhandling_loop(cf, mustPropagateResultValue)

   // alternative code fragment variations that have no "here" parameter
   // handy for "manual use", i.e. not compiler generated
  def _normal0            [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_normal            ((_here:N_code_normal            [R]) => cf, mustPropagateResultValue)
  def _threaded0          [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_threaded          ((_here:N_code_threaded          [R]) => cf, mustPropagateResultValue)
  def _unsure0            [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_unsure            ((_here:N_code_unsure            [R]) => cf, mustPropagateResultValue)
  def _tiny0              [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_tiny              ((_here:N_code_tiny              [R]) => cf, mustPropagateResultValue)
  def _eventhandling0     [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_eventhandling     ((_here:N_code_eventhandling     [R]) => cf, mustPropagateResultValue)
  def _eventhandling_loop0[R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_eventhandling_loop((_here:N_code_eventhandling_loop[R]) => cf, mustPropagateResultValue)

  implicit def _call      [R](calleeName: String, code: N_call[R] => ScriptNode[R], mustPropagateResultValue: Boolean = false) = T_call[R](calleeName, code, mustPropagateResultValue)
  
  implicit def valueToActualValueParameter[T<:Any](value: T) = new ActualValueParameter(value)

  //def _at[N<:CallGraphNode,T<:Child](_cf:N=>Unit)  
  //= (_child: T) => T_annotation[N,T]((here:N_annotation[N,T]) => _cf(here.there), _child)
 
  def _at[N<:CallGraphNode,T<:Child](_cf:N_annotation[N,T]=>Unit)  
  = (_child: T) => T_annotation[N,T]((here:N_annotation[N,T]) => _cf(here), _child)
 
  def _declare[T](name: Symbol) = new LocalVariable[T](name)
  
  // local variables need to be declared explicitly first; usage is as in:
  //  implicit def _key(_publisher: FormalInputParameter[Publisher], _keyCode: FormalConstrainedParameter[Char])  = {
  //    val _r = _declare[KeyPressScriptReactor[N_code_eh]]('r)      // <<== declaration
  //    _script('key, _publisher~'publisher, _keyCode~??'keyCode) {
  //     _seq( 
  //       _val(_r, (here:N_localvar[_]) => new KeyPressScriptReactor[N_code_eh](_publisher.value, _keyCode)),  // <<== initialisation
  //       _at{(there:N_code_eh) => {_r.at(there).value.subscribe(there); there.onDeactivate{_r.at(there).value.unsubscribe}; 
  //                                                                      there.onSuccess   {_r.at(there).value.acknowledgeEventHandled}}}
  //          (_eventhandling{})//{println("\nKey"+_keyCode.value)} // Temporary tracing
  //     )
  //    }
  //  }
  
  def _var     [V](v: LocalVariable[V]                             ) = T_localvar(false, false, v, null)
  def _var_loop[V](v: LocalVariable[V]                             ) = T_localvar(false,  true, v, null)
  def _var     [V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar(false, false, v, valueCode)
  def _val     [V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar( true, false, v, valueCode)
  def _var_loop[V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar(false,  true, v, valueCode)
  def _val_loop[V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar( true,  true, v, valueCode)

  def _privatevar[T<:Any](vsym: Symbol) = T_privatevar(vsym)

  // variants for operators with 0 to many operands
  //def _op0(opSymbol: String)                                                                      = T_0_ary(opSymbol)
  //def _op1(opSymbol: String)(c0: ChildNode)                                               = T_1_ary(opSymbol, c0)
  //def _op2(opSymbol: String)(c0: ChildNode, c1: ChildNode)                        = T_2_ary(opSymbol, c0, c1)
  //def _op3(opSymbol: String)(c0: ChildNode, c1: ChildNode, c2: ChildNode) = T_3_ary(opSymbol, c0, c1, c2)

  /* the following does not function well, as of Scala 2.10.
   * See https://issues.scala-lang.org/browse/SI-4176
   *
  def _op (opSymbol: String)(children: ChildNode*)                                        = T_n_ary(opSymbol, children:_*)
  
  def _seq               = _op(";")_
  def _alt               = _op ("+")_
  def _par               = _op ("&")_
  def _par_or            = _op ("|")_
  def _par_and2          = _op ("&&")_
  def _par_or2           = _op ("||")_
  def _par_equal         = _op ("==")_
  def _disrupt           = _op ("/")_
  def _shuffle           = _op ("%")_
  def _shuffle_1_or_more = _op ("%%")_
  def _seq_1_or_more     = _op (";%;")_
  def _interrupt         = _op ("%/")_
  def _interrupt_0_or_more = _op ("%/%/")_
  */
  
  def _op1(opSymbol: String)(child0  : Child ) = T_1_ary_op(opSymbol, child0)
  def _op (opSymbol: String)(children: Child*) = T_n_ary_op(opSymbol, children:_*)
  
  def _seq                (children: Child*) = _op(";"   )(children:_*)
  def _alt                (children: Child*) = _op("+"   )(children:_*)
  def _par                (children: Child*) = _op("&"   )(children:_*)
  def _par_or             (children: Child*) = _op("|"   )(children:_*)
  def _par_and2           (children: Child*) = _op("&&"  )(children:_*)
  def _par_or2            (children: Child*) = _op("||"  )(children:_*)
  def _par_equal          (children: Child*) = _op("=="  )(children:_*)
  def _disrupt            (children: Child*) = _op("/"   )(children:_*)
  def _shuffle            (children: Child*) = _op("%"   )(children:_*)
  def _shuffle_1_or_more  (children: Child*) = _op("%%"  )(children:_*)
  def _seq_1_or_more      (children: Child*) = _op(";%;" )(children:_*)
  def _interrupt          (children: Child*) = _op("%/"  )(children:_*)
  def _interrupt_0_or_more(children: Child*) = _op("%/%/")(children:_*)
  
  
  def _not           = _op1("!")_
  def _not_react     = _op1("-")_
  def _react         = _op1("~")_
  def _launch        = (child0: Child) => T_launch       (child0)
  def _launch_anchor = (child0: Child) => T_launch_anchor(child0)

  def _empty                                = T_epsilon            ()
  def _deadlock                             = T_delta              ()
  def _neutral                              = T_nu                 ()
  def _break                                = T_break              ()
  def _optionalBreak                        = T_optional_break     ()
  def _optionalBreak_loop                   = T_optional_break_loop()
  def _loop                                 = T_loop               ()
  def _while0  (_cond:         =>Boolean)   = T_while((here: N_while ) => _cond)
  def _while   (_cond:N_while  =>Boolean)   = T_while(_cond)
  def _if0     (_cond:         =>Boolean)(c0: Child) = T_if((here: N_if) => _cond, c0)
  def _if      (_cond:N_if     =>Boolean)(c0: Child) = T_if(_cond, c0)
  def _if_else0(_cond:         =>Boolean)(c0: Child, c1: Child) = T_if_else((here: N_if_else) => _cond, c0, c1)
  def _if_else (_cond:N_if_else=>Boolean)(c0: Child, c1: Child) = T_if_else(_cond, c0, c1)
  
  def _do_then     (c0: Child, c1: Child           )  = T_do_then     (c0, c1) 
  def _do_else     (c0: Child, c1: Child           )  = T_do_else     (c0, c1) 
  def _do_then_else(c0: Child, c1: Child, c2: Child)  = T_do_then_else(c0, c1, c2) 

  // we do not want scripts in DSL.scala, because a regular Scala compiler should be able to translate this. 
  // So we manually translate the following script:
  //
  //def script dataflow_then[T,U](s: Script[T], t: T=>Script[U]): U = 
  //     var s_node: N_call[T] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get)
  
  // Second parameter not t but _t; SubScript code generation quirk, should be undone.
  //def script dataflow_then[T](s: Script[T], _t: T=>ScriptNode[Any]): Any = 
  //     var s_node: N_call[Any] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get.asInstanceOf[T])
  //def script dataflow_then(s: Script[Any], t: Any=>Script[Any]): Any = 
  //     var s_node: N_call[Any] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get)
  

  def _dataflow_then[S,T](s: ScriptNode[S], t: S=>ScriptNode[T]): T_do_then = 
    {
       var s_node: N_call[S] = null
       //println(" _dataflow_then >> INIT")
       _do_then(_call("do",   (_n:N_call[S]) => {//println(" _dataflow_then >> _do_then do-part"); 
                                              s_node = _n; _n.calls(s.template, (s.p: _*)); s}), 
                _call("then", (_n:N_call[T]) => {//println(" _dataflow_then >> _do_then then-part: ${s_node.callee.$.get}"); 
                                              //println(s"s_node.callee.$$: ${s_node.callee.$}")
                                              val tp = t(s_node.$.get); 
                                              _n.calls(tp.template, (tp.p: _*)); tp}, true))
    }
  
  def _dataflow_then_else[S,T,U](s: ScriptNode[S], t: S=>ScriptNode[T], u: Throwable=>ScriptNode[U]): T_do_then_else = 
    {
       var s_node: N_call[S] = null
       //println(" _dataflow_then_else >> INIT")
       _do_then_else(_call("do", (_n:N_call[S])   => {//println(" _dataflow_then_else >> do-part"); 
                                                      s_node = _n; _n.calls(s.template, (s.p: _*)); s}), 
                     _call("then", (_n:N_call[T]) => {//println(" _dataflow_then_else >> then-part: ${s_node.callee.$.get}"); 
                                                      //println(s"s_node.callee.$$: ${s_node.callee.$}")
                                                      val tp = t(s_node.$.get); 
                                                      _n.calls(tp.template, (tp.p: _*)); tp}, true), 
                     _call("else", (_n:N_call[U]) => {//println(" _dataflow_then_else >> else-part: ${s_node.callee.$.get}"); 
                                                      //println(s"s_node.callee.$$: ${s_node.callee.$}")
                                                      val up = u(s_node.$failure); 
                                                      _n.calls(up.template, (up.p: _*)); up}, true))
    }
  
  def _dataflow_else[S,T,U](s: ScriptNode[S], u: Throwable=>ScriptNode[U]): T_do_else = 
    {
       var s_node: N_call[S] = null
       //println(" _dataflow_else >> INIT")
       _do_else(     _call("do", (_n:N_call[S])   => {//println(" _dataflow_else >> do-part"); 
                                                      s_node = _n; _n.calls(s.template, (s.p: _*)); s}), 
                     _call("else", (_n:N_call[U]) => {//println(" _dataflow_else >> else-part: ${s_node.callee.$.get}"); 
                                                      //println(s"s_node.callee.$$: ${s_node.callee.$}")
                                                      val up = u(s_node.$failure); 
                                                      _n.calls(up.template, (up.p: _*)); up}, true))
    }
  
  
 }