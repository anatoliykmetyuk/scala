/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: allow infix type patterns
//todo verify when stableId's should be just plain qualified type ids

package scala.tools.nsc
package ast.parser

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.reflect.internal.{ Precedence, ModifierFlags => Flags }
import scala.reflect.internal.Chars.{ isScalaLetter }
import scala.reflect.internal.util.{ SourceFile, Position, FreshNameCreator }
import Tokens._

/** Historical note: JavaParsers started life as a direct copy of Parsers
 *  but at a time when that Parsers had been replaced by a different one.
 *  Later it was dropped and the original Parsers reinstated, leaving us with
 *  massive duplication between Parsers and JavaParsers.
 *
 *  This trait and the similar one for Scanners/JavaScanners represents
 *  the beginnings of a campaign against this latest incursion by Cutty
 *  McPastington and his army of very similar soldiers.
 */
trait ParsersCommon extends ScannersCommon { self =>
  val global : Global
  // the use of currentUnit in the parser should be avoided as it might
  // cause unexpected behaviour when you work with two units at the
  // same time; use Parser.unit instead
  import global.{currentUnit => _, _}

  def newLiteral(const: Any) = Literal(Constant(const))
  def literalUnit            = gen.mkSyntheticUnit()

  /** This is now an abstract class, only to work around the optimizer:
   *  methods in traits are never inlined.
   */
  abstract class ParserCommon {
    val in: ScannerCommon
    def deprecationWarning(off: Offset, msg: String): Unit
    def accept(token: Token): Int

    /** Methods inParensOrError and similar take a second argument which, should
     *  the next token not be the expected opener (e.g. LPAREN) will be returned
     *  instead of the contents of7 the groupers.  However in all cases accept(LPAREN)
     *  will be called, so a parse error will still result.  If the grouping is
     *  optional, in.token should be tested before calling these methods.
     */
    @inline final def inParens[T](body: => T): T = {
      accept(LPAREN); val ret = body
      accept(RPAREN);     ret
    }
     
    @inline final def inParensOrError[T](body: => T, alt: T): T =
      if (in.token == LPAREN) inParens(body)
      else { accept(LPAREN) ; alt }

    @inline final def inParensOrUnit[T](body: => Tree): Tree = inParensOrError(body, literalUnit)
    @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

    @inline final def inBraces[T](body: => T): T = {
      accept(LBRACE); val ret = body
      accept(RBRACE); ret
    }
    @inline final def inBracesOrError[T](body: => T, alt: T): T =
      if (in.token == LBRACE) inBraces(body)
      else { accept(LBRACE) ; alt }

    @inline final def inBracesOrNil [T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
    @inline final def inBracesOrUnit[T](body: => Tree   ): Tree    = inBracesOrError(body, literalUnit)
    @inline final def dropAnyBraces [T](body: =>      T ):      T  = if (in.token == LBRACE) inBraces(body) else body

    @inline final def inBrackets[T](body: => T): T = {
      accept(LBRACKET); val ret = body
      accept(RBRACKET); ret
    }

    /** Creates an actual Parens node (only used during parsing.)
     */
    @inline final def makeParens(body: => List[Tree]): Parens = Parens(inParens(if (in.token == RPAREN) Nil else body))
  }
}

/** Performs the following context-free rewritings:
 *
 *  <ol>
 *    <li>
 *      Places all pattern variables in Bind nodes. In a pattern, for
 *      identifiers `x`:<pre>
 *                 x  => x @ _
 *               x:T  => x @ (_ : T)</pre>
 *    </li>
 *    <li>Removes pattern definitions (PatDef's) as follows:
 *      If pattern is a simple (typed) identifier:<pre>
 *        <b>val</b> x = e     ==>  <b>val</b> x = e
 *        <b>val</b> x: T = e  ==>  <b>val</b> x: T = e</pre>
 *
 *      if there are no variables in pattern<pre>
 *        <b>val</b> p = e  ==>  e match (case p => ())</pre>
 *
 *      if there is exactly one variable in pattern<pre>
 *        <b>val</b> x_1 = e <b>match</b> (case p => (x_1))</pre>
 *
 *      if there is more than one variable in pattern<pre>
 *        <b>val</b> p = e  ==>  <b>private synthetic val</b> t$ = e <b>match</b> (case p => (x_1, ..., x_N))
 *                        <b>val</b> x_1 = t$._1
 *                        ...
 *                        <b>val</b> x_N = t$._N</pre>
 *    </li>
 *    <li>
 *       Removes function types as follows:<pre>
 *        (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]</pre>
 *    </li>
 *    <li>
 *      Wraps naked case definitions in a match as follows:<pre>
 *        { cases }   ==>   (x => x.match {cases})<span style="font-family:normal;">, except when already argument to match</span></pre>
 *    </li>
 *  </ol>
 */
trait Parsers extends Scanners with MarkupParsers with ParsersCommon {
self =>
  val global: Global
  import global._

  case class OpInfo(lhs: Tree, operator: TermName, targs: List[Tree], offset: Offset) {
    def precedence = Precedence(operator.toString)
  }

  class SourceFileParser(val source: SourceFile) extends Parser {

    /** The parse starting point depends on whether the source file is self-contained:
     *  if not, the AST will be supplemented.
     */
    def parseStartRule =
      if (source.isSelfContained) () => compilationUnit()
      else () => scriptBody()

    def newScanner(): Scanner = new SourceFileScanner(source)

    val in = newScanner()
    in.init()

    def unit = global.currentUnit

    // suppress warnings; silent abort on errors
    def            warning(offset: Offset, msg: String) {}
    def deprecationWarning(offset: Offset, msg: String) {}

    def syntaxError(offset: Offset, msg: String): Unit = throw new MalformedInput(offset, msg)
    def incompleteInputError       (msg: String): Unit = throw new MalformedInput(source.content.length - 1, msg)

    object symbXMLBuilder extends SymbolicXMLBuilder(this, preserveWS = true) { // DEBUG choices
      val global: self.global.type = self.global
    }

    /** the markup parser
     * The first time this lazy val is accessed, we assume we were trying to parse an xml literal.
     * The current position is recorded for later error reporting if it turns out
     * that we don't have the xml library on the compilation classpath.
     */
    private[this] lazy val xmlp = {
      unit.encounteredXml(o2p(in.offset))
      new MarkupParser(this, preserveWS = true)
    }

    def scriptLiteral(doInBrackets: Boolean, simpleTermOnly: Boolean) : Tree = {
      val wasInSubScript_script     = in.isInSubScript_script
      val wasInSubScript_nativeCode = in.isInSubScript_nativeCode
      in.isInSubScript_script       = true
      in.isInSubScript_header       = false
      in.isInSubScript_nativeCode   = false
      var se: Tree = null
      
      if (simpleTermOnly)    se = simpleScriptTerm(allowParameterList = false)
      else if (doInBrackets) {
              in.scriptExpressionParenthesesNestingLevel += 1; inBrackets{se = scriptExpression()}
              in.scriptExpressionParenthesesNestingLevel -= 1
           }
           else                                                        se = scriptExpression() 
      
      val result = makeScriptHeaderAndLocalsAndBody("<lambda>", se, Nil, TypeTree())
      in.isInSubScript_script     = wasInSubScript_script
      in.isInSubScript_nativeCode = wasInSubScript_nativeCode
      result
    }
    def xmlLiteral       () : Tree = xmlp.xLiteral
    def xmlLiteralPattern() : Tree = xmlp.xLiteralPattern
  }

  class OutlineParser(source: SourceFile) extends SourceFileParser(source) {

    def skipBraces[T](body: T): T = {
      accept(LBRACE)
      var openBraces = 1
      while (in.token != EOF && openBraces > 0) {
        if (in.token == XMLSTART) xmlLiteral()
        else {
          if      (in.token == LBRACE) openBraces += 1
          else if (in.token == RBRACE) openBraces -= 1
          in.nextToken()
        }
      }
      body
    }

    override def blockExpr           (): Tree = skipBraces(EmptyTree)
    override def templateBody(isPre: Boolean) = skipBraces((noSelfType, EmptyTree.asList))
  }

  class UnitParser(override val unit: global.CompilationUnit, patches: List[BracePatch]) extends SourceFileParser(unit.source) { uself =>
    def this(unit: global.CompilationUnit) = this(unit, Nil)

    override def newScanner() = new UnitScanner(unit, patches)


    override def warning(offset: Offset, msg: String): Unit =
      reporter.warning(o2p(offset), msg)

    override def deprecationWarning(offset: Offset, msg: String): Unit =
      currentRun.reporting.deprecationWarning(o2p(offset), msg)


    private var smartParsing = false
    @inline private def withSmartParsing[T](body: => T): T = {
      val saved = smartParsing
      smartParsing = true
      try body
      finally smartParsing = saved
    }
    def withPatches(patches: List[BracePatch]): UnitParser = new UnitParser(unit, patches)

    val syntaxErrors = new ListBuffer[(Int, String)]
    def showSyntaxErrors() =
      for ((offset, msg) <- syntaxErrors)
        reporter.error(o2p(offset), msg)

    override def syntaxError(offset: Offset, msg: String): Unit = {
      if (smartParsing) syntaxErrors += ((offset, msg))
      else reporter.error(o2p(offset), msg)
    }

    override def incompleteInputError(msg: String): Unit = {
      val offset = source.content.length - 1
      if (smartParsing) syntaxErrors += ((offset, msg))
      else currentRun.parsing.incompleteInputError(o2p(offset), msg)
    }

    /** parse unit. If there are inbalanced braces,
     *  try to correct them and reparse.
     */
    def smartParse(): Tree = withSmartParsing {
      val firstTry = parse()
      if (syntaxErrors.isEmpty) firstTry
      else in.healBraces() match {
        case Nil      => showSyntaxErrors() ; firstTry
        case patches  => (this withPatches patches).parse()
      }
    }
  }

  type Location = Int
  final val Local     : Location = 0
  final val InBlock   : Location = 1
  final val InTemplate: Location = 2

  // These symbols may not yet be loaded (e.g. in the ide) so don't go
  // through definitions to obtain the names.
  lazy val ScalaValueClassNames = Seq(tpnme.AnyVal,
      tpnme.Unit,
      tpnme.Boolean,
      tpnme.Byte,
      tpnme.Short,
      tpnme.Char,
      tpnme.Int,
      tpnme.Long,
      tpnme.Float,
      tpnme.Double)

  import nme.raw

  abstract class Parser extends ParserCommon { parser =>
    val in: Scanner
    def unit: CompilationUnit
    def source: SourceFile

    /** Scoping operator used to temporarily look into the future.
     *  Backs up scanner data before evaluating a block and restores it after.
     */
    @inline final def lookingAhead[T](body: => T): T = {
      val saved = new ScannerData {} copyFrom in
      in.nextToken()
      try body finally in copyFrom saved
    }

    /** Perform an operation while peeking ahead.
     *  Pushback if the operation yields an empty tree or blows to pieces.
     */
    @inline def peekingAhead(tree: =>Tree): Tree = {
      @inline def peekahead() = {
        in.prev copyFrom in
        in.nextToken()
      }
      @inline def pushback() = {
        in.next copyFrom in
        in copyFrom in.prev
      }
      peekahead()
      // try it, in case it is recoverable
      val res = try tree catch { case e: Exception => pushback() ; throw e }
      if (res.isEmpty) pushback()
      res
    }

    class ParserTreeBuilder extends TreeBuilder {
      val global: self.global.type = self.global
      def unit = parser.unit
      def source = parser.source
    }
    val treeBuilder = new ParserTreeBuilder
    import treeBuilder.{global => _, unit => _, source => _, fresh => _, _}

    implicit def fresh: FreshNameCreator = unit.fresh

    def o2p(offset: Offset                          ): Position = Position.offset(source, offset)
    def r2p( start: Offset, mid: Offset, end: Offset): Position = rangePos(source, start, mid, end)
    def r2p( start: Offset, mid: Offset             ): Position = r2p(start, mid, in.lastOffset max start)
    def r2p(offset: Offset                          ): Position = r2p(offset, offset)

    /** whether a non-continuable syntax error has been seen */
    private var lastErrorOffset : Int = -1

    /** The types of the context bounds of type parameters of the surrounding class
     */
    private var classContextBounds: List[Tree] = Nil
    @inline private def savingClassContextBounds[T](op: => T): T = {
      val saved = classContextBounds
      try op
      finally classContextBounds = saved
    }


    /** Are we inside the Scala package? Set for files that start with package scala
     */
    private var inScalaPackage = false
    private var currentPackage = ""

            def      resetPackage() {inScalaPackage = false; currentPackage = ""}
    private def inScalaRootPackage = inScalaPackage       && currentPackage == "scala"

    def parseStartRule: () => Tree

    def parseRule[T](rule: this.type => T): T = {
      val t = rule(this)
      accept(EOF)
      t
    }

    /** This is the general parse entry point.
     */
    def parse(): Tree = parseRule(_.parseStartRule())

    /** These are alternative entry points for repl, script runner, toolbox and parsing in macros.
     */
    def parseStats(): List[Tree] = parseRule(_.templateStats())
    def parseStatsOrPackages(): List[Tree] = parseRule(_.templateOrTopStatSeq())

    /** This is the parse entry point for code which is not self-contained, e.g.
     *  a script which is a series of template statements.  They will be
     *  swaddled in Trees until the AST is equivalent to the one returned
     *  by compilationUnit().
     */
    def scriptBody(): Tree = {
      val stmts = parseStats()

      def mainModuleName = newTermName(settings.script.value)
      /* If there is only a single object template in the file and it has a
       * suitable main method, we will use it rather than building another object
       * around it.  Since objects are loaded lazily the whole script would have
       * been a no-op, so we're not taking much liberty.
       */
      def searchForMain(): Option[Tree] = {
        /* Have to be fairly liberal about what constitutes a main method since
         * nothing has been typed yet - for instance we can't assume the parameter
         * type will look exactly like "Array[String]" as it could have been renamed
         * via import, etc.
         */
        def isMainMethod(t: Tree) = t match {
          case DefDef(_, nme.main, Nil, List(_), _, _)  => true
          case _                                        => false
        }
        /* For now we require there only be one top level object. */
        var seenModule = false
        val newStmts   = stmts collect {
          case t @ Import(_, _) => t
          case md @ ModuleDef(mods, name, template) if !seenModule && (md exists isMainMethod) =>
            seenModule = true
            /* This slightly hacky situation arises because we have no way to communicate
             * back to the scriptrunner what the name of the program is.  Even if we were
             * willing to take the sketchy route of settings.script.value = progName, that
             * does not work when using fsc.  And to find out in advance would impose a
             * whole additional parse.  So instead, if the actual object's name differs from
             * what the script is expecting, we transform it to match.
             */
            if (name == mainModuleName) md
            else treeCopy.ModuleDef(md, mods, mainModuleName, template)
          case _ =>
            /* If we see anything but the above, fail. */
            return None
        }
        Some(makeEmptyPackage(0, newStmts))
      }

      if (mainModuleName == newTermName(ScriptRunner.defaultScriptMain))
        searchForMain() foreach { return _ }

      /*  Here we are building an AST representing the following source fiction,
       *  where `moduleName` is from -Xscript (defaults to "Main") and <stmts> are
       *  the result of parsing the script file.
       *
       *  {{{
       *  object moduleName {
       *    def main(args: Array[String]): Unit =
       *      new AnyRef {
       *        stmts
       *      }
       *  }
       *  }}}
       */
      def emptyInit   = DefDef(
        NoMods,
        nme.CONSTRUCTOR,
        Nil,
        ListOfNil,
        TypeTree(),
        Block(List(Apply(gen.mkSuperInitCall, Nil)), literalUnit)
      )

      // def main
      def mainParamType = AppliedTypeTree(Ident(tpnme.Array), List(Ident(tpnme.String)))
      def mainParameter = List(ValDef(Modifiers(Flags.PARAM), nme.args, mainParamType, EmptyTree))
      def mainDef       = DefDef(NoMods, nme.main, Nil, List(mainParameter), scalaDot(tpnme.Unit), gen.mkAnonymousNew(stmts))

      // object Main
      def moduleName  = newTermName(ScriptRunner scriptMain settings)
      def moduleBody  = Template(atInPos(scalaAnyRefConstr) :: Nil, noSelfType, List(emptyInit, mainDef))
      def moduleDef   = ModuleDef(NoMods, moduleName, moduleBody)

      // package <empty> { ... }
      makeEmptyPackage(0, moduleDef :: Nil)
    }

/* --------------- PLACEHOLDERS ------------------------------------------- */

    /** The implicit parameters introduced by `_` in the current expression.
     *  Parameters appear in reverse order.
     */
    var placeholderParams: List[ValDef] = Nil

    /** The placeholderTypes introduced by `_` in the current type.
     *  Parameters appear in reverse order.
     */
    var placeholderTypes: List[TypeDef] = Nil

    def checkNoEscapingPlaceholders[T](op: => T): T = {
      val savedPlaceholderParams = placeholderParams; placeholderParams = List()
      val savedPlaceholderTypes  = placeholderTypes ; placeholderTypes  = List()
      val res = op

      placeholderParams match {case vd::_ => syntaxError(vd.pos, "unbound placeholder parameter", skipIt = false); placeholderParams = List() case _ =>}
      placeholderTypes  match {case td::_ => syntaxError(td.pos, "unbound wildcard type"        , skipIt = false); placeholderTypes  = List() case _ =>}
      placeholderParams = savedPlaceholderParams
      placeholderTypes  = savedPlaceholderTypes

      res
    }

    def placeholderTypeBoundary(op: => Tree): Tree = {
      val savedPlaceholderTypes = placeholderTypes
      placeholderTypes = List()
      var t = op
      if (!placeholderTypes.isEmpty && t.isInstanceOf[AppliedTypeTree]) {
        val expos = t.pos
        ensureNonOverlapping(t, placeholderTypes)
        t = atPos(expos) { ExistentialTypeTree(t, placeholderTypes.reverse) }
        placeholderTypes = List()
      }
      placeholderTypes = placeholderTypes ::: savedPlaceholderTypes
      t
    }

    def isWildcard(t: Tree): Boolean = t match {
      case Ident    (name1) => !placeholderParams.isEmpty && name1 == placeholderParams.head.name
      case Typed    (t1, _) => isWildcard(t1)
      case Annotated(t1, _) => isWildcard(t1)
      case _                => false
    }

/* ------------- ERROR HANDLING ------------------------------------------- */

    val assumedClosingParens = mutable.Map (RPAREN          -> 0, 
                                            RBRACKET        -> 0, 
                                            RBRACE          -> 0,
                                            RBRACE_DOT      -> 0,
                                            RBRACE_DOT3     -> 0,
                                            RBRACE_QMARK    -> 0,
                                            RBRACE_EMARK    -> 0,
                                            RBRACE_ASTERISK -> 0,
                                            RBRACE_CARET    -> 0)

    private var inFunReturnType = false
    @inline private def fromWithinReturnType[T](body: => T): T = {
      val saved = inFunReturnType
      inFunReturnType = true
      try body
      finally inFunReturnType = saved
    }

    protected def skip(targetToken: Token): Unit = {
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF            => return
          case SEMI           => if (nparens == 0 && nbraces == 0) return
          case NEWLINE        => if (nparens == 0 && nbraces == 0) return
          case NEWLINES       => if (nparens == 0 && nbraces == 0) return
          case RPAREN
             | RPAREN_ASTERISK 
             | RPAREN_ASTERISK2 =>                                         nparens -= 1
          case LPAREN
             | LPAREN_ASTERISK 
             | LPAREN_ASTERISK2 =>                                         nparens += 1
          case RBRACE            
	         | RBRACE_DOT              
	         | RBRACE_DOT3         
	         | RBRACE_QMARK   
	         | RBRACE_EMARK                              
	         | RBRACE_ASTERISK                             
	         | RBRACE_CARET   => if (nbraces == 0) {
	                               // the parser did not eat any token when e.g. "*}" was expected but "}" seen. 
	                               // Therefore eat the token in case of such a mismatch:
                                   targetToken match {
							       case RBRACE            
								      | RBRACE_DOT              
								      | RBRACE_DOT3         
								      | RBRACE_QMARK   
								      | RBRACE_EMARK                              
								      | RBRACE_ASTERISK                             
								      | RBRACE_CARET   => in.nextToken()
							       case _ =>
                                   }
	                               return
	                             }
	                             nbraces -= 1
          case LBRACE            
	         | LBRACE_DOT              
	         | LBRACE_DOT3         
	         | LBRACE_QMARK   
	         | LBRACE_EMARK                              
	         | LBRACE_ASTERISK                             
	         | LBRACE_CARET   =>                                           nbraces += 1
          case _              =>
        }
        if (targetToken == in.token && nparens == 0 && nbraces == 0) return
        in.nextToken()
      }
    }
    def warning(offset: Offset, msg: String): Unit
    def incompleteInputError(msg: String): Unit

    private def syntaxError(pos:  Position, msg: String, skipIt: Boolean) {syntaxError(pos pointOrElse in.offset, msg, skipIt)}
    def         syntaxError(offset: Offset, msg: String                 ): Unit
    def         syntaxError(                msg: String, skipIt: Boolean) {syntaxError(in.offset, msg, skipIt) /*; Thread.dumpStack*/}

    def syntaxError(offset: Offset, msg: String, skipIt: Boolean): Unit = {
      if (offset > lastErrorOffset) {
        syntaxError(offset, msg)
        lastErrorOffset = in.offset         // no more errors on this token.
      }
      if (skipIt) skip(UNDEF)
    }

    def warning(msg: String): Unit = warning(in.offset, msg)

    def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean) {
      if (in.token == EOF)  incompleteInputError(msg)
      else                  syntaxError(in.offset, msg, skipIt)
    }
    def syntaxErrorOrIncompleteAnd[T](msg: String, skipIt: Boolean)(and: T): T = {
      syntaxErrorOrIncomplete(msg, skipIt)
      and
    }

    def expectedMsgTemplate(exp: String, fnd: String) = s"$exp expected but $fnd found."
    def expectedKWIdentMsg(kw: TermName): String = {
      val s = if (in.token==IDENTIFIER) s"${in.token} (${token2string(in.token)})" else token2string(in.token)
      expectedMsgTemplate(kw.toString(), s)
    }
    def expectedMsg(token: Token): String = expectedMsgTemplate(token2string(token), token2string(in.token))

    /** Consume one token of the specified type, or signal an error if it is not there. */
    def accept(token: Token): Offset = {
      val offset = in.offset
      if (  token != in.token) {syntaxErrorOrIncomplete(expectedMsg(token), skipIt = false)
        if (token == RPAREN 
        ||  token == RBRACE    
        ||  token == RBRACKET 
        ||  token == RBRACE_DOT     
        ||  token == RBRACE_DOT3    
        ||  token == RBRACE_QMARK   
        ||  token == RBRACE_EMARK   
        ||  token == RBRACE_ASTERISK
        ||  token == RBRACE_CARET)
          if (in.parenBalance(token) + assumedClosingParens(token) < 0)
                                       assumedClosingParens(token) += 1
          else  skip(token)
        else    skip(UNDEF)
      }
      if (in.token == token) in.nextToken()
      offset
    }
    def acceptIdent(kw: TermName): Offset = {
      val offset = in.offset
      if ( IDENTIFIER != in.token
      ||   kw         != in.name) syntaxErrorOrIncomplete(expectedKWIdentMsg(kw), skipIt = false)
      else in.nextToken()
      offset
    }


    /** {{{
     *  semi = nl {nl} | `;`
     *  nl  = `\n' // where allowed
     *  }}}
     */
    def acceptStatSep(): Unit = in.token match {
      case NEWLINE | NEWLINES                 => in.nextToken()
      case _ if (in.prevWasInSubScript_script 
             &&  in.afterLineEnd())           =>       // Note: this needs be tested
      case _                                  => accept(SEMI)
                                 
    }
    def acceptStatSepOpt()    = if (!isStatSeqEnd) acceptStatSep()

    def errorTypeTree    = setInPos(TypeTree() setType ErrorType)
    def errorTermTree    = setInPos(newLiteral(null))
    def errorPatternTree = setInPos(Ident(nme.WILDCARD))

    /** Check that type parameter is not by name or repeated. */
    def checkNotByNameOrVarargs(tpt: Tree) = {
      if (treeInfo isByNameParamType tpt)
        syntaxError(tpt.pos, "no by-name parameter type allowed here", skipIt = false)
      else if (treeInfo isRepeatedParamType tpt)
        syntaxError(tpt.pos, "no * parameter type allowed here", skipIt = false)
    }

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | PRIVATE |
           PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
      case _ => false
    }

    def isAnnotation: Boolean = in.token == AT

    def isLocalModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
      case _ => false
    }

    def isTemplateIntro: Boolean = in.token match {
      case OBJECT | CASEOBJECT | CLASS | CASECLASS | TRAIT  => true
      case _                                                => false
    }
    def isDclIntro: Boolean = in.token match {
      case VAL | VAR | DEF | TYPE => true
      case _                      => false
    }

    def isDefIntro = isTemplateIntro || isDclIntro

    def isNumericLit: Boolean = in.token match {
      case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => true
      case _ => false
    }

    def isIdentExcept(except: Name) = isIdent && in.name != except
    def isIdentOf(name: Name)       = isIdent && in.name == name

    def isUnaryOp  = isIdent && raw.isUnary(in.name)
    def isRawStar  = isRawIdent && in.name == raw.STAR
    def isRawBar   = isRawIdent && in.name == raw.BAR
    def isRawIdent = in.token == IDENTIFIER

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
    def isMacro = in.token == IDENTIFIER && in.name == nme.MACROkw

    def isLiteralToken(token: Token) = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | INTERPOLATIONID | SYMBOLLIT | TRUE | FALSE | NULL => true
      case _                                                             => false
    }
    def isLiteral = isLiteralToken(in.token)

    def isExprIntroToken(token: Token): Boolean = isLiteralToken(token) || (token match {
      case IDENTIFIER | BACKQUOTED_IDENT |
           THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
           DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
      case _ => false
    })

    def isExprIntro: Boolean = isExprIntroToken(in.token)

    def isTypeIntroToken(token: Token): Boolean = token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS |
           SUPER | USCORE | LPAREN | AT =>  true
      case _                            => false
    }

    def isTokenAClosingBrace(token: Int): Boolean = 
      if (token==RBRACE) true
      else if (!in.isInSubScript_nativeCode) false
      else token match {
      case RBRACE_DOT     
         | RBRACE_DOT3    
         | RBRACE_QMARK   
         | RBRACE_EMARK   
         | RBRACE_ASTERISK
         | RBRACE_CARET   => true
      case _              => false
      }

    def isStatSeqEnd                     = isTokenAClosingBrace(in.token)             || in.token == EOF  || in.token == ARROW2 || in.token == GREATER2
    def isCaseDefEnd                     = in.token == RBRACE || in.token == CASE     || in.token == EOF  || in.token == ARROW2 || in.token == GREATER2
    def isStatSep(token: Token): Boolean =    token == NEWLINE ||   token == NEWLINES ||    token == SEMI
    def isStatSep              : Boolean = isStatSep(in.token)


/* --------- COMMENT AND ATTRIBUTE COLLECTION ----------------------------- */

    /** A hook for joining the comment associated with a definition.
     *  Overridden by scaladoc.
     */
    def joinComment(trees: => List[Tree]): List[Tree] = trees

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

    def atPos[T <: Tree](offset: Offset                            )(t: T): T = atPos(r2p(offset))(t)
    def atPos[T <: Tree]( start: Offset, point: Offset             )(t: T): T = atPos(r2p(start, point))(t)
    def atPos[T <: Tree]( start: Offset, point: Offset, end: Offset)(t: T): T = atPos(r2p(start, point, end))(t)
    def atPos[T <: Tree](pos: Position                             )(t: T): T = global.atPos(pos)(t)

    def  atInPos[T <: Tree](t: T): T =    atPos(o2p(in.offset))(t)
    def setInPos[T <: Tree](t: T): T = t setPos o2p(in.offset)

    /** Convert tree to formal parameter list. */
    def convertToParams(tree: Tree): List[ValDef] = tree match {
      case Parens(ts) => ts map convertToParam
      case _          => List(convertToParam(tree))
    }

    /** Convert tree to formal parameter. */
    def convertToParam(tree: Tree): ValDef = atPos(tree.pos) {
      def removeAsPlaceholder(name: Name): Unit = {
        placeholderParams = placeholderParams filter (_.name != name)
      }
      def errorParam = makeParam(nme.ERROR, errorTypeTree setPos o2p(tree.pos.end))
      tree match {
        case       Ident(name)                     => removeAsPlaceholder(name); makeParam(name.toTermName, TypeTree() setPos o2p(tree.pos.end))
        case Typed(Ident(name), tpe) if tpe.isType => removeAsPlaceholder(name); makeParam(name.toTermName, tpe) // get the ident!
        case build.SyntacticTuple(as) =>
          val arity = as.length
          val example = analyzer.exampleTuplePattern(as map { case Ident(name) => name; case _ => nme.EMPTY })
          val msg =
            sm"""|not a legal formal parameter.
                 |Note: Tuples cannot be directly destructured in method or function parameters.
                 |      Either create a single parameter accepting the Tuple${arity},
                 |      or consider a pattern matching anonymous function: `{ case $example => ... }"""
          syntaxError(tree.pos, msg, skipIt = false)
          errorParam
        case _ =>
          syntaxError(tree.pos, "not a legal formal parameter", skipIt = false)
          errorParam
      }
    }

    /** Convert (qual)ident to type identifier. */
    def convertToTypeId(tree: Tree): Tree = atPos(tree.pos) {
      convertToTypeName(tree) getOrElse {
        syntaxError(tree.pos, "identifier expected", skipIt = false)
        errorTypeTree
      }
    }

    /** {{{ part { `sep` part } }}},or if sepFirst is true, {{{ { `sep` part } }}}. */
    final def tokenSeparated[T](separator: Token, sepFirst: Boolean, part: => T): List[T] = {
      val ts = new ListBuffer[T]
      if (!sepFirst) ts += part
      while (in.token == separator) {in.nextToken(); ts += part}
      ts.toList
    }
    @inline final def commaSeparated[T](part: => T): List[T] = tokenSeparated(COMMA, sepFirst = false, part)
    @inline final def caseSeparated[T](part: => T   ): List[T   ] = tokenSeparated(CASE, sepFirst = true, part)
    def               readAnnots      (part: => Tree): List[Tree] = tokenSeparated(AT  , sepFirst = true, part)

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

    /** Modes for infix types. */
    object InfixMode extends Enumeration {val FirstOp, LeftOp, RightOp = Value}

    var opstack: List[OpInfo] = Nil

    @deprecated("Use `scala.reflect.internal.Precedence`", "2.11.0")
    def precedence(operator: Name): Int = Precedence(operator.toString).level

    private def opHead = opstack.head
    private def headPrecedence = opHead.precedence
    private def popOpInfo(): OpInfo = try opHead finally opstack = opstack.tail
    private def pushOpInfo(top: Tree): Unit = {
      val name   = in.name
      val offset = in.offset
      ident()
      val targs = if (in.token == LBRACKET) exprTypeArgs() else Nil
      val opinfo = OpInfo(top, name, targs, offset)
      opstack ::= opinfo
    }

    def checkHeadAssoc(leftAssoc: Boolean) = checkAssoc(opHead.offset, opHead.operator, leftAssoc)
    def checkAssoc(offset: Offset, op: Name, leftAssoc: Boolean) = (
      if (treeInfo.isLeftAssoc(op) != leftAssoc)
        syntaxError(offset, "left- and right-associative operators with same precedence may not be mixed", skipIt = false)
    )

    def finishPostfixOp(start: Int, base: List[OpInfo], opinfo: OpInfo): Tree = {
      if (opinfo.targs.nonEmpty)
        syntaxError(opinfo.offset, "type application is not allowed for postfix operators")

      val od = stripParens(reduceExprStack(base, opinfo.lhs))
      makePostfixSelect(start, opinfo.offset, od, opinfo.operator)
    }

    def finishBinaryOp(isExpr: Boolean, opinfo: OpInfo, rhs: Tree): Tree = {
      import opinfo._
      val operatorPos: Position = Position.range(rhs.pos.source, offset, offset, offset + operator.length)
      val pos                   = lhs.pos union rhs.pos union operatorPos withPoint offset

      atPos(pos)(makeBinop(isExpr, lhs, operator, rhs, operatorPos, opinfo.targs))
    }

    def reduceExprStack(base: List[OpInfo], top: Tree): Tree    = reduceStack(isExpr = true, base, top)
    def reducePatternStack(base: List[OpInfo], top: Tree): Tree = reduceStack(isExpr = false, base, top)

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top: Tree): Tree = {
      val opPrecedence = if (isIdent) Precedence(in.name.toString) else Precedence(0)
      val leftAssoc    = !isIdent || (treeInfo isLeftAssoc in.name)

      reduceStack(isExpr, base, top, opPrecedence, leftAssoc)
    }

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top: Tree, opPrecedence: Precedence, leftAssoc: Boolean): Tree = {
      def isDone          = opstack == base
      def lowerPrecedence = !isDone && (opPrecedence < headPrecedence)
      def samePrecedence  = !isDone && (opPrecedence == headPrecedence)
      def canReduce       = lowerPrecedence || leftAssoc && samePrecedence

      if (samePrecedence)
        checkHeadAssoc(leftAssoc)

      def loop(top: Tree): Tree = if (canReduce) {
        val info = popOpInfo()
        if (!isExpr && info.targs.nonEmpty) {
          syntaxError(info.offset, "type application is not allowed in pattern")
          info.targs.foreach(_.setType(ErrorType))
        }
        loop(finishBinaryOp(isExpr, info, top))
      } else top

      loop(top)
    }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    /** Methods which implicitly propagate the context in which they were
     *  called: either in a pattern context or not.  Formerly, this was
     *  threaded through numerous methods as boolean isPattern.
     */
    trait PatternContextSensitive {
      /** {{{
       *  ArgType       ::=  Type
       *  }}}
       */
      def         argType(): Tree
      def functionArgType(): Tree

      private def tupleInfixType(start: Offset) = {
        in.nextToken()
        if (in.token == RPAREN) {
          in.nextToken()
          atPos(start, accept(ARROW)) { makeFunctionTypeTree(Nil, typ()) }
        }
        else {
          val ts = functionTypes()
          accept(RPAREN)
          if (in.token == ARROW)
            atPos(start, in.skipToken()) { makeFunctionTypeTree(ts, typ()) }
          else {
            ts foreach checkNotByNameOrVarargs
            val tuple = atPos(start) { makeTupleType(ts) }
            infixTypeRest(
              compoundTypeRest(
                annotTypeRest(
                  simpleTypeRest(
                    tuple))),
              InfixMode.FirstOp
            )
          }
        }
      }
      private def makeExistentialTypeTree(t: Tree) = {
        // EmptyTrees in the result of refinement() stand for parse errors
        // so it's okay for us to filter them out here
        ExistentialTypeTree(t, refinement() flatMap {
          case t @ TypeDef(_, _, _, TypeBoundsTree(_, _)) => Some(t)
          case t @ ValDef(_, _, _, EmptyTree) => Some(t)
          case EmptyTree => None
          case _ => syntaxError(t.pos, "not a legal existential clause", skipIt = false); None
        })
      }

      /** {{{
       *  Type ::= InfixType `=>' Type
       *         | `(' [`=>' Type] `)' `=>' Type
       *         | InfixType [ExistentialClause]
       *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
       *  ExistentialDcl    ::= type TypeDcl | val ValDcl
       *  }}}
       */
      def typ(): Tree = placeholderTypeBoundary {
        val start = in.offset
        val t     = if (in.token == LPAREN) tupleInfixType(start)
                    else                         infixType(InfixMode.FirstOp)

        in.token match {
          case ARROW    => atPos(start, in.skipToken()) { makeFunctionTypeTree(List(t), typ()) }
          case FORSOME  => atPos(start, in.skipToken()) { makeExistentialTypeTree(t) }
          case _        => t
        }
      }

      /** {{{
       *  TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
       *  }}}
       */
      def typeArgs(): List[Tree] = inBrackets(types())

      /** {{{
       *  AnnotType        ::=  SimpleType {Annotation}
       *  }}}
       */
      def annotType(): Tree = placeholderTypeBoundary { annotTypeRest(simpleType()) }

      /** {{{
       *  SimpleType       ::=  SimpleType TypeArgs
       *                     |  SimpleType `#' Id
       *                     |  StableId
       *                     |  Path `.' type
       *                     |  `(' Types `)'
       *                     |  WildcardType
       *  }}}
       */
      def simpleType(): Tree = {
        val start = in.offset
        simpleTypeRest(in.token match {
          case LPAREN   => atPos(start)(makeTupleType(inParens(types())))
          case USCORE   => wildcardType(in.skipToken())
          case _        =>
            path(thisOK = false, typeOK = true) match {
              case r @ SingletonTypeTree(_) => r
              case r => convertToTypeId(r)
            }
        })
      }

      private def typeProjection(t: Tree): Tree = {
        val hashOffset = in.skipToken()
        val nameOffset = in.offset
        val name       = identForType(skipIt = false)
        val point      = if (name == tpnme.ERROR) hashOffset else nameOffset
        atPos(t.pos.start, point)(SelectFromTypeTree(t, name))
      }
      def simpleTypeRest(t: Tree): Tree = in.token match {
        case HASH     => simpleTypeRest(typeProjection(t))
        case LBRACKET => simpleTypeRest(atPos(t.pos.start, t.pos.point)(AppliedTypeTree(t, typeArgs())))
        case _        => t
      }

      /** {{{
       *  CompoundType ::= AnnotType {with AnnotType} [Refinement]
       *                |  Refinement
       *  }}}
       */
      def compoundType(): Tree = compoundTypeRest(
        if (in.token == LBRACE) atInPos(scalaAnyRefConstr)
        else annotType()
      )

      def compoundTypeRest(t: Tree): Tree = {
        val ts = new ListBuffer[Tree] += t
        while (in.token == WITH) {
          in.nextToken()
          ts += annotType()
        }
        newLineOptWhenFollowedBy(LBRACE)
        val types         = ts.toList
        val braceOffset   = in.offset
        val hasRefinement = in.token == LBRACE
        val refinements   = if (hasRefinement) refinement() else Nil
        // Warn if they are attempting to refine Unit; we can't be certain it's
        // scala.Unit they're refining because at this point all we have is an
        // identifier, but at a later stage we lose the ability to tell an empty
        // refinement from no refinement at all.  See bug #284.
        if (hasRefinement) types match {
          case Ident(name) :: Nil if name endsWith "Unit" => warning(braceOffset, "Detected apparent refinement of Unit; are you missing an '=' sign?")
          case _                                          =>
        }
        // The second case includes an empty refinement - refinements is empty, but
        // it still gets a CompoundTypeTree.
        ts.toList match {
          case tp :: Nil if !hasRefinement => tp  // single type, no refinement, already positioned
          case tps                         => atPos(t.pos.start)(CompoundTypeTree(Template(tps, noSelfType, refinements)))
        }
      }

      def infixTypeRest(t: Tree, mode: InfixMode.Value): Tree = {
        // Detect postfix star for repeated args.
        // Only RPAREN can follow, but accept COMMA and EQUALS for error's sake.
        // Take RBRACE as a paren typo.
        def checkRepeatedParam = if (isRawStar) {
          lookingAhead (in.token match {
            case RPAREN | COMMA | EQUALS | RBRACE => t
            case _                                => EmptyTree
          })
        } else EmptyTree
        def asInfix = {
          val opOffset  = in.offset
          val leftAssoc = treeInfo.isLeftAssoc(in.name)
          if (mode != InfixMode.FirstOp)
            checkAssoc(opOffset, in.name, leftAssoc = mode == InfixMode.LeftOp)
          val tycon = atPos(opOffset) { Ident(identForType()) }
          newLineOptWhenFollowing(isTypeIntroToken)
          def mkOp(t1: Tree) = atPos(t.pos.start, opOffset) { AppliedTypeTree(tycon, List(t, t1)) }
          if (leftAssoc)
            infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
          else
            mkOp(infixType(InfixMode.RightOp))
        }
        if (isIdent) checkRepeatedParam orElse asInfix
        else t
      }

      /** {{{
       *  InfixType ::= CompoundType {id [nl] CompoundType}
       *  }}}
       */
      def infixType(mode: InfixMode.Value): Tree = placeholderTypeBoundary { infixTypeRest(compoundType(), mode) }

      /** {{{
       *  Types ::= Type {`,' Type}
       *  }}}
       */
      def         types(): List[Tree] = commaSeparated(argType())
      def functionTypes(): List[Tree] = commaSeparated(functionArgType())
    }

    /** Assumed (provisionally) to be TermNames. */
    def ident(skipIt: Boolean): Name = (
      if (isIdent) {
        val name = in.name.encode
        in.nextToken()
        name
      }
      else syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
    )

    def ident(): Name = ident(skipIt = true)
    def rawIdent(): Name = try in.name finally in.nextToken()

    /** For when it's known already to be a type name. */
    def identForType(               ): TypeName = ident(      ).toTypeName
    def identForType(skipIt: Boolean): TypeName = ident(skipIt).toTypeName

    def identOrMacro(): Name = if (isMacro) rawIdent() else ident()

    def selector(t: Tree): Tree = {
      val point = if(isIdent) in.offset else in.lastOffset //SI-8459
      //assert(t.pos.isDefined, t)
      if (t != EmptyTree) Select(t, ident(skipIt = false)) setPos r2p(t.pos.start, point, in.lastOffset)
      else errorTermTree // has already been reported
    }

    /** {{{
     *  Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     *  }}}
     */
    def path(thisOK: Boolean, typeOK: Boolean): Tree = {
      val start = in.offset
      var t: Tree = null
      if (in.token == THIS) {
        in.nextToken();      t = atPos(start) { This(tpnme.EMPTY) }
        if (!thisOK 
        || in.token == DOT) {t = selectors(t, typeOK, accept(DOT))}
      } 
      else if (in.token == SUPER) {
        in.nextToken();      t = atPos(start) { Super(This(tpnme.EMPTY), mixinQualifierOpt()) }
        accept(DOT)   ;      t = selector(t)
        if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
      } else {
        val tok  = in.token
        val name = ident()
        t = atPos(start) {
          if (tok == BACKQUOTED_IDENT) Ident(name) updateAttachment BackquotedIdentifierAttachment
          else Ident(name)
        }
        if (in.token == DOT) {
          val dotOffset = in.skipToken()
          if (in.token == THIS) {
            in.nextToken();      t = atPos(start) { This(name.toTypeName) }
            if (!thisOK 
            || in.token == DOT)  t = selectors(t, typeOK, accept(DOT))
          } 
          else if (in.token == SUPER) {
            in.nextToken();      t = atPos(start) { Super(This(name.toTypeName), mixinQualifierOpt()) }
            accept(DOT)   ;      t = selector(t)
            if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
          } 
          else {                 t = selectors(t, typeOK, dotOffset)}
        }
      }
      t
    }

    def selectors(t: Tree, typeOK: Boolean, dotOffset: Offset): Tree =
      if (typeOK && in.token == TYPE) {
        in.nextToken()
        atPos(t.pos.start, dotOffset) { SingletonTypeTree(t) }
      }
      else {
        val t1 = selector(t)
        if (in.token == DOT) { selectors(t1, typeOK, in.skipToken()) }
        else t1
      }

    /** {{{
    *   MixinQualifier ::= `[' Id `]'
    *   }}}
    */
    def mixinQualifierOpt(): TypeName =
      if (in.token == LBRACKET) inBrackets(identForType())
      else tpnme.EMPTY

    /** {{{
     *  StableId ::= Id
     *            |  Path `.' Id
     *            |  [id `.'] super [`[' id `]']`.' id
     *  }}}
     */
    def stableId(): Tree =
      path(thisOK = false, typeOK = false)

    /** {{{
    *   QualId ::= Id {`.' Id}
    *   }}}
    */
    def qualId(): Tree = {
      val start = in.offset
      val id = atPos(start) { Ident(ident()) }
      if (in.token == DOT) { selectors(id, typeOK = false, in.skipToken()) }
      else id
    }
    /** Calls `qualId()` and manages some package state. */
    private def pkgQualId() = {
      if (in.token == IDENTIFIER && in.name.encode == nme.scala_)
        inScalaPackage = true

      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)

      if (currentPackage == "") currentPackage = pkg.toString
      else                      currentPackage = currentPackage + "." + pkg

      pkg
    }

    /** {{{
     *  SimpleExpr    ::= literal
     *                  | symbol
     *                  | null
     *  }}}
     */
    def literal(isNegated: Boolean = false, inPattern: Boolean = false, start: Offset = in.offset): Tree = atPos(start) {
      def finish(value: Any): Tree = try newLiteral(value) finally in.nextToken()
      if (in.token == SYMBOLLIT)
        Apply(scalaDot(nme.Symbol), List(finish(in.strVal)))
      else if (in.token == INTERPOLATIONID)
        interpolatedString(inPattern = inPattern)
      else finish(in.token match {
        case CHARLIT                => in. charVal
        case INTLIT                 => in.  intVal(isNegated).toInt
        case LONGLIT                => in.  intVal(isNegated)
        case FLOATLIT               => in.floatVal(isNegated).toFloat
        case DOUBLELIT              => in.floatVal(isNegated)
        case STRINGLIT | STRINGPART => in.  strVal.intern()
        case TRUE                   => true
        case FALSE                  => false
        case NULL                   => null
        case _                      => syntaxErrorOrIncompleteAnd("illegal literal", skipIt = true)(null)
      })
    }

    /** Handle placeholder syntax.
     *  If evaluating the tree produces placeholders, then make it a function.
     */
    private def withPlaceholders(tree: =>Tree, isAny: Boolean): Tree = {
      val savedPlaceholderParams = placeholderParams
      placeholderParams = List()
      var res = tree
      if (placeholderParams.nonEmpty && !isWildcard(res)) {
        res = atPos(res.pos)(Function(placeholderParams.reverse, res))
        if (isAny) placeholderParams foreach (_.tpt match {
          case tpt @ TypeTree() => tpt setType definitions.AnyTpe
          case _                => // some ascription
        })
        placeholderParams = List()
      }
      placeholderParams = placeholderParams ::: savedPlaceholderParams
      res
    }

    /** Consume a USCORE and create a fresh synthetic placeholder param. */
    private def freshPlaceholder(): Tree = {
      val start = in.offset
      val pname = freshTermName()
      in.nextToken()
      val id = atPos(start)(Ident(pname))
      val param = atPos(id.pos.focus)(gen.mkSyntheticParam(pname.toTermName))
      placeholderParams = param :: placeholderParams
      id
    }

    private def interpolatedString(inPattern: Boolean): Tree = {
      def errpolation() = syntaxErrorOrIncompleteAnd("error in interpolated string: identifier or block expected",
                                                     skipIt = true)(EmptyTree)
      // Like Swiss cheese, with holes
      def stringCheese: Tree = atPos(in.offset) {
        val start        = in.offset
        val interpolator = in.name.encoded // ident() for INTERPOLATIONID

        val partsBuf = new ListBuffer[Tree]
        val exprsBuf = new ListBuffer[Tree]
        in.nextToken()
        while (in.token == STRINGPART) {
          partsBuf += literal()
          exprsBuf += (
            if (inPattern) dropAnyBraces(pattern())
            else in.token match {
              case IDENTIFIER => atPos(in.offset)(Ident(ident()))
              //case USCORE   => freshPlaceholder()  // ifonly etapolation
              case LBRACE     => expr()              // dropAnyBraces(expr0(Local))
              case THIS       => in.nextToken(); atPos(in.offset)(This(tpnme.EMPTY))
              case _          => errpolation()
            }
          )
        }
        if (in.token == STRINGLIT) partsBuf += literal()

      // Documenting that it is intentional that the ident is not rooted for purposes of virtualization
      //val t1 = atPos(o2p(start)) { Select(Select (Ident(nme.ROOTPKG), nme.scala_), nme.StringContext) }
        val t1 = atPos(o2p(start)) { Ident(nme.StringContext) }
        val t2 = atPos(start) { Apply(t1, partsBuf.toList) }
        t2 setPos t2.pos.makeTransparent
        val t3 = Select(t2, interpolator) setPos t2.pos
        atPos(start) { Apply(t3, exprsBuf.toList) }
      }
      if (inPattern) stringCheese
      else withPlaceholders(stringCheese, isAny = true) // strinterpolator params are Any* by definition
    }

/* ------------- NEW LINES ------------------------------------------------- */

    def newLineOpt () {if (in.token == NEWLINE) in.nextToken()}
    def newLinesOpt() {if (in.token == NEWLINE || in.token == NEWLINES) in.nextToken()}

    def newLineOptWhenFollowedBy(token: Offset): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Token => Boolean): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }
    def newLineOptWhenFollowing_TokenData(p: TokenData => Boolean) {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next)) newLineOpt()
    }

/* ------------- TYPES ---------------------------------------------------- */

    /** {{{
     *  TypedOpt ::= [`:' Type]
     *  }}}
     */
    def typedOpt       ()                  : Tree = if (in.token == COLON) {in.nextToken(); typ()} else TypeTree()
    def typeOrInfixType(location: Location): Tree = if (location == Local)                  typ()  else startInfixType()
    def annotTypeRest  (t:            Tree): Tree = (t /: annotations(skipNewLines = false)) (makeAnnotated)

    /** {{{
     *  WildcardType ::= `_' TypeBounds
     *  }}}
     */
    def wildcardType(start: Offset) = {
      val pname = freshTypeName("_$")
      val t     = atPos(start)(Ident(pname))
      val bounds = typeBounds()
      val param  = atPos(t.pos union bounds.pos) { makeSyntheticTypeParam(pname, bounds) }
      placeholderTypes = param :: placeholderTypes
      t
    }

/* ----------- SUBSCRIPT -------------------------------------------- */

    /*
     * TBD: 
     * 
     * formal parameters: p ==> p.value, using Transformer
     * method resolution += ScriptApply resolution: phase 2
     *   - if !resolved type is script then encapsulate: _normal{here:N_code_normal => ...}
     *   - ? implicit with multiple parameters?
     * 
     * TBTested: 
     * 
     * @, if, while, blocks, specials -> functions
     * actual parameters: box & add constraints
     * 
     * DONE:
     * 
     * acceptStatSepOpt: not for scripts...in.previousToken NL is also ok
     * 
     * end scripts sections & script definitions appropriately
     *   script.. section ends when line indentation <= to "script.." indentation & appropriate token
     *   else current script specification ends when line indentation < "=" & appropriate token  
     * reduce stack: make n-ary op...
     * formal parameters: box
     * script method = body ===> script method = script(name, params){body}
     */
    
    val NEWLINE_Name  = newTermName("NEWLINE")
    val   SPACE_Name  = newTermName(in.raw_space)
    val    SEMI_Name  = newTermName(";")
    val       CURLYARROW2_Name = newTermName("~~>")
    val CURLYBROKENARROW2_Name = newTermName("~/~>")
    
    val _script_Name  = newTermName("_script")
    val  script_Name  = newTermName("script")
    val      at_Name  = newTermName("at")
    val   value_Name  = newTermName("value")
    val    here_Name  = newTermName("here")
    val   _node_Name  = newTermName("_node")
    val   there_Name  = newTermName("there")
    val     tmp_Name  = newTermName("$tmp")
    val    tmp1_Name  = newTermName("$tmp1")
    val  sender_Name  = newTermName("sender")
    val MsgSCRIPT_Name: TermName = newTermName("r$")

    val            bind_inParam_Name  = newTermName(scala.reflect.NameTransformer.encode("~"))
    val           bind_outParam_Name  = newTermName(scala.reflect.NameTransformer.encode("~?"))
    val   bind_constrainedParam_Name  = newTermName(scala.reflect.NameTransformer.encode("~??"))
    
    val      formalOutputParameter_Name = newTypeName("FormalOutputParameter")
    val formalConstrainedParameter_Name = newTypeName("FormalConstrainedParameter")
    val       actualValueParameter_Name = newTermName("ActualValueParameter")
    val      actualOutputParameter_Name = newTermName("ActualOutputParameter")
    val actualConstrainedParameter_Name = newTermName("ActualConstrainedParameter")
    val    actualAdaptingParameter_Name = newTermName("ActualAdaptingParameter")

    val   SPACE_Ident = Ident(  SPACE_Name)
    val NEWLINE_Ident = Ident(NEWLINE_Name)
    
    def    here_Ident = Ident( here_Name) // Note: such items should be def's rather than val's; else the Typer will get confused
    def   there_Ident = Ident(there_Name)
    def   _node_Ident = Ident(_node_Name)
                                                     
    val nameScala       = newTermName("scala")
    val nameSubScript   = newTermName("subscript")
    val nameDSL         = newTermName("DSL")
    val nameVM          = newTermName("vm")
    val name_scriptNodeType = newTypeName("ScriptNode")
    val name_scriptType = newTypeName("Script")
    val name_unitType   = newTypeName("Unit")
 
    def sSubScriptDSL: Tree = Select(Ident(nameSubScript), nameDSL)
    def sSubScriptVM : Tree = Select(Ident(nameSubScript), nameVM )
    
    // only for annotations: subscript.vm.model.template.concrete
    val nameModel         = newTermName("model")
    val nameTemplate      = newTermName("template")
    val nameConcrete      = newTermName("concrete")
    def sSubScriptVMModel                 : Tree = Select(sSubScriptVM             , nameModel   )
    def sSubScriptVMModelTemplate         : Tree = Select(sSubScriptVMModel        , nameTemplate)
    def sSubScriptVMModelTemplateConcrete : Tree = Select(sSubScriptVMModelTemplate, nameConcrete)
    
    def sActualValueParameter      : Tree = Select(sSubScriptVM,       actualValueParameter_Name)
    def sFormalOutputParameter     : Tree = Select(sSubScriptVM,      formalOutputParameter_Name)
    def sFormalConstrainedParameter: Tree = Select(sSubScriptVM, formalConstrainedParameter_Name)

    def sActualOutputParameter     : Tree = Select(sSubScriptVM,      actualOutputParameter_Name)
    def sActualConstrainedParameter: Tree = Select(sSubScriptVM, actualConstrainedParameter_Name)
    def sActualAdaptingParameter   : Tree = Select(sSubScriptVM,    actualAdaptingParameter_Name)

    def s_Unit       : Tree = Select(Ident(nameScala), name_unitType)
    def s__script    : Tree = Select(sSubScriptDSL, _script_Name)
    def s_scriptNodeType : Tree = Select(sSubScriptVM, name_scriptNodeType)
    def s_scriptType : Tree = Select(sSubScriptVM, name_scriptType)
  //def s_scriptType : Tree = AppliedTypeTree(s_scriptType0, List(s_Unit))
    
    val Caret_Name = newTermName("^")
    
    def isSubScriptUnaryPrefixOp (tokenData: TokenData): Boolean = in.isSubScriptUnaryPrefixOp(tokenData)
    def isSubScriptPostfixOp     (tokenData: TokenData): Boolean = in.isSubScriptPostfixOp    (tokenData)
    
    def isSubScriptInfixOp(tokenData: TokenData): Boolean = in.isSubScriptInfixOp(tokenData)
    def isSubScriptOperator(name: Name): Boolean = in.isSubScriptOperator(name.toString)
    
    def isSubScriptOperator(tree: Tree): Boolean = {val result = tree match {
      case NEWLINE_Ident | SPACE_Ident => true
      case Ident(NEWLINE_Name) => true // TBD: cleanup (the problem is: NEWLINE handling may follow 2 paths, in scriptExpression()
      case Ident(name: Name) => isSubScriptOperator(name)
      case _ => false
      }
//println(s"isSubScriptOperator($tree): $result")
      result
    }
    
    @inline final def inSubscriptArgumentParens[T](body: => T): T = {
      accept(LPAREN); in.isInSubScript_nativeCode =  true; val ret = body 
                      in.isInSubScript_nativeCode = false 
      accept(RPAREN); 
      ret
    }
    def  makeParameterTransferFunction(exp: Tree): Tree = {
      val pname   = freshTermName()
      val id      = atPos(exp.pos) (Ident(pname))
      val param   = atPos(id.pos.focus){ gen.mkSyntheticParam(pname.toTermName) }
      val vparams = List(param)
      Function(vparams , Assign(exp, id))
    }

      
//  def       makeFormalInputParameter(typer: Tree): Tree = AppliedTypeTree(convertToTypeId(      sFormalInputParameter), List(typer))
    def      makeFormalOutputParameter(typer: Tree): Tree = AppliedTypeTree(convertToTypeId(     sFormalOutputParameter), List(typer))
    def makeFormalConstrainedParameter(typer: Tree): Tree = AppliedTypeTree(convertToTypeId(sFormalConstrainedParameter), List(typer))
    
    def      makeActualOutputParameter(exp   : Tree, constraint: Tree = null): Tree = {
      if (constraint==null) Apply(sActualOutputParameter, List(exp, makeParameterTransferFunction(exp)))
      else             Apply(sActualConstrainedParameter, List(exp, makeParameterTransferFunction(exp), constraint)) 
    }
    
    def makeActualAdaptingParameter(param: Tree, constraint: Tree): Tree = {
      param match {
         case Ident(formalParamName) => makeActualAdaptingParameter(formalParamName, constraint)
         case _ => syntaxError(in.offset, "An adapting parameter should be a name of a formal constrained parameter");
                   param
      }
    }
    def    makeActualAdaptingParameter(paramName: Name, constraint: Tree = null): Tree = {
       if (constraint==null) Apply(sActualAdaptingParameter, List(Ident(newTermName(underscore_prefix(paramName.toString)))))
       else  Apply(sActualAdaptingParameter, List(Ident(newTermName(underscore_prefix(paramName.toString))), constraint))
    }
    def underscore_prefix(s: String) = "_"+s
	def underscore_TermName(n: TermName) = newTermName(underscore_prefix(n.toString))

    /*
     * Enclose the given block with a function with parameter "here" (or "script") of the given node type 
     * i.e.: 
     *         here: NodeType => body
     * 
     * Actually, we add implicitness this item, through an extra value:
     * 
     *         _node: NodeType => implicit val here=_node; body
     * 
     */
    def blockToFunction(body: Tree, nodeType: Tree, pos: Position, hereOrScript: TermName): Function = {
      val vparams = List(
          atPos(pos) {
            makeParam(_node_Name, nodeType setPos pos)
          }
      )
      val implicitVal          = atPos(pos) {ValDef(Modifiers(Flags.IMPLICIT), hereOrScript, TypeTree(), _node_Ident)}
      val implicitVal_seq_body = makeBlock(List(implicitVal,body))
      
      Function(vparams , implicitVal_seq_body)
    }
    
    // answer Script[scriptResultType]
	  def scriptNodeType_resultType(scriptResultType: Tree) = AppliedTypeTree(s_scriptNodeType, List(scriptResultType))
  
    /*
     * Enclose the given block with a function with parameter "here" or "there" of the given node type 
     * i.e.: here: NodeType => block
     * 
     * FTTB at the call sides of these 2 blockToFunction methods, many of these NodeType's are parameterized with [Any].
     * Ideally the type parameter should be the return type of the given block
     * Probably this can be done, but in the Typer phase. 
     * That would be done when this Parser is cleaned up again, or replaced by SugarScala.
     * 
     * Beware: (FTTB) blockToFunction had been copied to Typers.scala
     */
    def blockToFunction_here  (block: Tree, nodeType: Tree, pos: Position): Function = blockToFunction(block, nodeType, pos,  here_Name)
    def blockToFunction_there (block: Tree, nodeType: Tree, pos: Position): Function = blockToFunction(block, nodeType, pos, there_Name) //  TBD Clean up
    def blockToFunction_script(block: Tree, 
                                    scriptResultType: Tree, pos: Position): Function = {blockToFunction(block,TypeTree(),pos,script_Name) //  TBD Clean up
    }
    //{ val vparams = List(makeParam(there_Name, TypeTree()))
    //  Function(vparams , block)
    //}
    
    val DSLFunName_Dataflow_then      = newTermName("_dataflow_then")
    val DSLFunName_Dataflow_else      = newTermName("_dataflow_else")
    val DSLFunName_Dataflow_then_else = newTermName("_dataflow_then_else")
    
    def subScriptDSLFunForDataflow_then     : Tree = Select(sSubScriptDSL, DSLFunName_Dataflow_then)
    def subScriptDSLFunForDataflow_else     : Tree = Select(sSubScriptDSL, DSLFunName_Dataflow_else)
    def subScriptDSLFunForDataflow_then_else: Tree = Select(sSubScriptDSL, DSLFunName_Dataflow_then_else)

    def subScriptDSLFunForOperator(op: Tree, spaceOp: Name, newlineOp: Name): Tree = {
      var n: Name = null
      val operatorName      : Name = op match {
                                        case   SPACE_Ident       => spaceOp
                                        case NEWLINE_Ident       => newlineOp
                                        case Ident(NEWLINE_Name) => newlineOp // TBD: cleanup, see def isSubScriptOperator(tree: Tree)
                                        case Ident(name:Name)    => name
                                        }
      val operatorDSLFunName: Name = mapOperatorNameToDSLFunName(operatorName) 
      
      //println(s"subScriptDSLFunForOperator($n >> $operatorName >>> $operatorDSLFunName)")                                        

      Select(sSubScriptDSL, operatorDSLFunName)
    }
    
    val mapOperatorStringToDSLFunString = Map[String,String](
        ";"  -> "seq",
        "+"  -> "alt",
        "&"  -> "par",
        "&&" -> "par_and2",
        "|"  -> "par_or",
        "||" -> "par_or2",
        "/"  -> "disrupt"        
    )
    val mapOperatorNameToDSLFunName = mapOperatorStringToDSLFunString map {case(k,v) => (newTermName(k): Name, newTermName("_"+v): Name)}
    
    val mapTokenToDSLFunString = Map[Int,String](
        LPAREN_ASTERISK  -> "launch",
        LPAREN_ASTERISK2 -> "launch_anchor",
        LBRACE          -> "normal",
        LBRACE_ASTERISK -> "threaded",
        LBRACE_QMARK    -> "unsure",
        LBRACE_EMARK    -> "tiny",
        LBRACE_DOT      -> "eventhandling",
        LBRACE_DOT3     -> "eventhandling_loop",        
        AT              -> "at"    ,     
        WHILE           -> "while"    ,     
        IF              -> "if"       ,
        ELSE            -> "if_else"  ,    
        DO_THEN         -> "do_then"  ,
        DO_ELSE         -> "do_else"  ,
        DO_THEN_ELSE    -> "do_then_else",
        DEF             -> "declare" ,
        VAL             -> "val" ,
        VAR             -> "var" ,
        LPAREN_PLUS_RPAREN       -> "empty"       ,
        LPAREN_MINUS_RPAREN      -> "deadlock"    ,
        LPAREN_PLUS_MINUS_RPAREN -> "neutral"     ,
        LPAREN_SEMI_RPAREN       -> "skip"        ,
        DOT             -> "optionalBreak"        ,
        DOT2            -> "optionalBreak_loop"   ,
        DOT3            -> "loop"
    )
    val mapTokenToDSLFunName = mapTokenToDSLFunString map {case(k,v) => (k, newTermName("_"+v): Name)}
    val break_Name = newTermName("_break")
    
    val mapTokenToVMNodeString = Map[Int,String](
        LPAREN_ASTERISK          -> "launch",
        LPAREN_ASTERISK2         -> "launch_anchor",
        LBRACE                   -> "code_normal",
        LBRACE_ASTERISK          -> "code_threaded",
        LBRACE_QMARK             -> "code_unsure",
        LBRACE_EMARK             -> "code_tiny",
        LBRACE_DOT               -> "code_eventhandling",
        LBRACE_DOT3              -> "code_eventhandling_loop",
        IF                       -> "if"       ,
        ELSE                     -> "if_else"  ,
        DO_THEN                  -> "do_then"  ,
        DO_ELSE                  -> "do_else"  ,
        DO_THEN_ELSE             -> "do_then_else",
        VAL                      -> "localvar" ,
        VAR                      -> "localvar" ,
        0                        -> "CallGraphNodeTrait_1",

        // unused:
        AT                       -> "annotation"         ,
        DOT                      -> "optional_break"     ,
        DOT2                     -> "optional_break_loop",
        DOT3                     -> "loop"               ,
        LPAREN_PLUS_RPAREN       -> "epsilon"            ,
        LPAREN_MINUS_RPAREN      -> "delta"              ,
        LPAREN_PLUS_MINUS_RPAREN -> "nu"                 ,
        WHILE                    -> "while"       
    )
    val mapTokenToVMNodeTypeName     = mapTokenToVMNodeString map {case(k,v) => (k, newTypeName("N_"+v): Name)}
    val mapTokenToVMTemplateTypeName = mapTokenToVMNodeString map {case(k,v) => (k, newTypeName("T_"+v): Name)} // tmp, used for annotations
    
    def dslFunForBreak       : Select = Select(sSubScriptDSL, break_Name)
    def dslFunFor(token: Int): Select = Select(sSubScriptDSL, mapTokenToDSLFunName(token))
    def vmNodeFor(token: Int): Select = Select(sSubScriptVM , mapTokenToVMNodeTypeName(token))
    
    def vmNodeForCall_Any = AppliedTypeTree(Select(sSubScriptVM , newTypeName("N_call")), List(Ident(any_TypeName)))
    
    val      any_TypeName = newTypeName("Any")
    val function_TypeName = newTypeName("Function")

    def eatNewlines(): Boolean = {
      if (in.token==NEWLINE || in.token==NEWLINES) {
        in.nextToken()
        true
      }
      false
    }
    
    def isScriptIdent = in.token == IDENTIFIER && in.name == nme.SCRIPTkw // TBD: use script_Name instead?
    def isBreakIdent  = in.token == IDENTIFIER && in.name == nme.BREAKkw
    
    def subScriptInfixOpPrecedence(operatorName: Name): Int = {
      val result =
      if      (operatorName eq nme.ERROR) -1
      else if (operatorName eq NEWLINE_Name) 1
      else operatorName.startChar match {
      case ';'             => 2
      case '|'             => 3
      case '^'             => 4
      case '&'             => 5
      case '=' | '!'       => 6
      case '<' | '>'       => 7
      case ':'             => 8
      case '+' | '-'       => 9
      case '*' | '/' | '%' => 10
      case _               => 11
      }
      //println(s"subScriptInfixOpPrecedence($operatorName): $result")   
      result
    }
    
/* 
 * FTTB: no communication, channels, try, catch, throw, match, for, resultHandler
 
 naryOperatorDesignator  =++ ";"   "-;"
                             "||"  "|"
                             orParArrow
                             "|+"  "|;"   "|/"
                             "||+" "||;"  "||/"
                             "|+|" "|;|"  "|/|"
                             "&&"  "&"
                             andParArrow
                             "=="
                             "+"
                             "/"  "%"  "%/"  "%/%/"  "%&"  "%;"
                             "·"
                             ("?" simpleValueExpression ":")
 
  andParArrow             =  "&~~>" +  "&~~{" scalaCode "}~>" +  "&~~(" scalaTupel ")~~>"
                          + "&&~~>" + "&&~~{" scalaCode "}~>" + "&&~~(" scalaTupel ")~~>"
 
  orParArrow              =  "|~~>" +  "|~~{" scalaCode "}~>" +  "|~~(" scalaTupel ")~~>"
                          + "||~~>" + "||~~{" scalaCode "}~>" + "||~~(" scalaTupel ")~~>"
 
  channelName_dots        =++ "<-->"   "<~~>"
                              "<-.->"  "<~.~>"
                              "<-..->" "<~..~>"
                              "<-->.." "<~~>.."
  
  simpleArrow             =+ "<-" "<~" "->" "~>"
 
  arrow                   =+ simpleArrow "<-*" "<~*" "*->" "*~>" "?->" "?~>"
 
  simpleValueExpression   = "_"
                          + literal
                          + "new" (classTemplate + templateBody)
                          + ( "here"
                            + currentInstanceExpression 
                            + identifier . "." currentInstanceExpression )
                            (.. "." identifier) 
 
  currentInstanceExpression = "this" + "super" "." identifier
 */
    
/*
  subScriptCode           = . "override" & . "implicit"; "script"; 
                                    scriptDefinition
                            + ".." (scriptDefinition..)
 
  scriptDefinition        = scriptHeader . ("+=" + "=") scriptExpression
 
  scriptHeader            = scriptName_dots  (.formalParameterList) (.":" typer) .. ","
                        |+| channelName_dots (.formalParameterList) (.":" typer) 
 
  scriptName_dots         = scriptName . ".."  + ("."+"..") scriptName
  channelName_dots        = (.identifier) doubleArrow_dots
 
  scriptName              = identifier %; simpleArrow
 
  identifiers             =       identifier      .. ","
 
  formalParameterList     = "(" (.formalParameters) ")"
  formalParameters        =       formalParameter .. ","
  formalParameter         = identifier ":" typer  . formalOutputMarker
  formalOutputMarker      = "?" + "??"
 
 */

    def      isFormalOutputParameter (p: ValDef): Boolean =      isFormalOutputParameter(p.name)
    def isFormalConstrainedParameter (p: ValDef): Boolean = isFormalConstrainedParameter(p.name)
    def      isFormalOutputParameter (p: Name  ): Boolean = scriptFormalOutputParameters     .contains(p.toString)
    def isFormalConstrainedParameter (p: Name  ): Boolean = scriptFormalConstrainedParameters.contains(p.toString)

    def storeScriptFormalOutputParameter      (name: Name, tpt: Tree) {scriptFormalOutputParameters      += name.toString->tpt}
    def storeScriptFormalConstrainedParameter (name: Name, tpt: Tree) {scriptFormalConstrainedParameters += name.toString->tpt}
    
    @inline final def inScriptParens[T<:Tree](lparen: Int, body: => T): Tree = {
      val rparen = lparen match {case LPAREN           => RPAREN 
                                 case LPAREN_ASTERISK  => RPAREN_ASTERISK 
                                 case LPAREN_ASTERISK2 => RPAREN_ASTERISK2}
      accept(lparen); in.scriptExpressionParenthesesNestingLevel += 1; val ret = body
      accept(rparen); in.scriptExpressionParenthesesNestingLevel -= 1; 
      
      if (lparen==LPAREN) ret 
      else Apply(dslFunFor(lparen), List(ret))
    }
    
    // the following aims to provide a context for Script data.
    // it is a quick hack; 
    // context sensitive transformations should be moved to a later compiler phase
    var scriptFormalOutputParameters      = new scala.collection.mutable.HashMap[String,Tree]
    var scriptFormalConstrainedParameters = new scala.collection.mutable.HashMap[String,Tree]
    
    // These are maps from Name to pairs of Tree and Boolean. Here's what Boolean means:
    // 1) true - Tree IS a type definition that can be used out-of-the-box
    // 2) false - Tree is NOT a type definition. It is a value, for which this local variable is declared,
    //            but who's type is still unknown.
    var scriptLocalVariables              = new scala.collection.mutable.HashMap[Name,(Tree, Boolean)] // should be in a context stack; now the scope becomes too big 
    var scriptLocalValues                 = new scala.collection.mutable.HashMap[Name,(Tree, Boolean)]
    
    def makeScriptHeaderAndLocalsAndBody(name: String, scriptBody: Tree, paramBindings: List[Tree], resultType: Tree): Tree = {
    		        
		        // now all parameters and local values should be available in the list buffers.
		        // transform the tree so that the identifiers are replaced appropriately
		        // Note: actual adapting parameters have already got an underscore in their name prefix (...)
		        // so these will not be found in the scriptFormalOutputParameters list etc.
		        val scriptLocalDataTransformer = new Transformer {
		          override def transform(tree: Tree): Tree = {
		            //println(s"transforming: $tree")
		            tree match {
		            case ident @ Ident(name) => 
		              if      (isFormalOutputParameter      (name)
		                    || isFormalConstrainedParameter (name)) atPos(ident.pos) {Select(Ident(newTermName(underscore_prefix(name.toString))), value_Name)} // _p.value
		              else if (scriptLocalVariables.contains(name) 
		                   ||  scriptLocalValues   .contains(name)) {  // _c.at(here).value
		                      /*val select_at     = atPos(ident.pos) {Select(Ident(newTermName(underscore_prefix(name.toString))), at_Name)}
		                      val apply_at_here = atPos(ident.pos) {Apply(select_at, List(here_Ident))}
		                      atPos(ident.pos) {Select(apply_at_here, value_Name)}
		                      */
		                      atPos(ident.pos)(ScriptVal(name))
		              } else ident
		            case _ => super.transform(tree)
		            }
		          }
		        }
		        
		        // check whether the script has a single code fragment or script apply
		        // then the 
		        
		        object CounterCodeFragmentsAndScriptApplies extends Transformer {
		          var count = 0
		          override def transform(tree: Tree): Tree = {
		            tree match {
		            case ScriptApply(_,_,_) | ScriptCodeFragment(_,_) => count += 1; tree
		            case _ => super.transform(tree)
		            }
		          }
		          def doCount(tree: Tree): Int = {transform(tree); count}
		        }
		        object MarkerResultPropagations extends Transformer {
		          var doPropagate = false
		          override def transform(tree: Tree): Tree = {
		            //println(s"MarkerResultPropagations doPropagate: $doPropagate tree: $tree")
		            tree match {
		              case Apply(Apply(TypeApply(Select(_,_dataflow_then), _),_),_) => tree // a script lambda; has already been processed
		              case ScriptUserElement(Caret_Name, body,_,_) => val   oldDoPropagate = doPropagate
		                                                              try     {doPropagate = true; transform(body)}
		                                                              finally {doPropagate = oldDoPropagate}
		              case ScriptApply(fun, args, _) => 
		                 //if (doPropagate) println(s"MarkerResultPropagations doPropagate=true; tree: $tree")
		                 //println(s"MarkerResultPropagations s@ScriptApply: $s s.mustPropagateResultValue: ${s.mustPropagateResultValue}")
		                 atPos(tree.pos) {ScriptApply(fun, args, doPropagate)}
		              case ScriptCodeFragment(token, code) => 
                    val parameterizedType = AppliedTypeTree(vmNodeFor(token), List(Ident(any_TypeName)))
                    Apply(dslFunFor(token), List(blockToFunction_here(code, parameterizedType, tree.pos), newLiteral(doPropagate))) // TBD: transform here?

		              case _ => super.transform(tree)
		            }
		          }
		          def mark(tree:Tree): Tree = {val count = CounterCodeFragmentsAndScriptApplies.doCount(scriptBody)
		                                       doPropagate = count==1
		                                       //println(s"MarkerResultPropagations script: $name count=$count doPropagate=$doPropagate")
		                                       transform(tree)
		                                      }
		        }
            //println(s"Before MarkerResultPropagations: $scriptBody")
		        val scriptBody_markedResultPropagations        = MarkerResultPropagations.mark(scriptBody)
            //println(s"After MarkerResultPropagations: $scriptBody_markedResultPropagations")

		        val rhs_withAdjustedScriptLocalDataTransformed = scriptLocalDataTransformer.transform(scriptBody_markedResultPropagations)
		        
		        // Make rhsMethod: (script: Script[resultType]) => rhs_withAdjustedScriptLocalDataTransformed
		        val rhsMethod   = blockToFunction_script(rhs_withAdjustedScriptLocalDataTransformed, resultType, scriptBody.pos)
		        
		        // structure for local variables, the script header and its body
		        val resultElems = new ListBuffer[Tree]
		        
		        // add for each variable and value: val _c = subscript.DSL._declare[Char]('c)
		        
		        // This pattern captures the algorithm of the local variable's body generation
		        // First argument is the name of the variable, second - its type, that makes sense
		        // in current scope
		        def localValBody(vn: Name, vt: Tree): Tree = {
		          val vSym               = Apply(scalaDot(nme.Symbol), List(Literal(Constant(vn.toString))))
                  val declare_typed  = TypeApply      (dslFunFor(DEF), List(vt))
                  val rhs            = Apply(declare_typed, List(vSym))
                  rhs
		        }
		        
		        // This pattern captures the local variable declaration.
		        // First algorithm is the name, second algorithm is the body
		        def localValDef(vn: Name, rhs: Tree): ValDef = {
		          val underscored_v_name = newTermName(underscore_prefix(vn.toString))
		          val valDef = ValDef(NoMods, underscored_v_name, TypeTree(), rhs)
		          valDef
		        }
		        
		        import TypeOperations._
		        for ((vn,(vt, isType)) <- scriptLocalVariables ++ scriptLocalValues) {
		          val valDef = {
		            val rhs = if (isType) localValBody(vn, vt) else withTypeOf(vt)(localValBody(vn, _))
		            localValDef(vn, rhs)
		          }
		          resultElems += valDef
		        }
		        
		        // the final part...
		        
		        val scriptNameAsSym         = Apply(scalaDot(nme.Symbol), List(Literal(Constant(name.toString))))
		        val dslScriptTyped          = TypeApply(s__script, List(resultType))
	          val scriptHeader            = Apply(dslScriptTyped, This(tpnme.EMPTY)::scriptNameAsSym::paramBindings)   // _script(this, `name, _p~`p...)
	          val scriptHeaderAndBody     = Apply(scriptHeader, List(rhsMethod))
	            
	          resultElems += scriptHeaderAndBody
	          makeBlock(resultElems.toList)
    }
    
    def scriptDefsOrDcls(start : Int, mods: Modifiers): List[Tree] = {
      in.isInSubScript_script = true
      in.isInSubScript_header = true
      in.nextToken
      val result = new ListBuffer[Tree]
      val doMultipleScripts = in.token == DOT2
      if (doMultipleScripts) {
        // TBD: forbid tab characters in such sections, at least in script headers and in top level script expressions
        in.nextToken()
      }
      while (in.token==NEWLINE
         ||  in.token==NEWLINES) {
//println(s"scriptDefsOrDcls skip NL token: ${in.token}")            
        in.nextToken()
      }

//println(s"scriptDefsOrDcls doMultipleScripts: $doMultipleScripts in.token: ${in.token}")            
      // loop; exit when no ident or ident with line pos <= linePosOfScriptsSection
         
      var mustExit = false
      while (!mustExit)
      {
        in.isInSubScript_header = true
        
	      if (doMultipleScripts) {
          //if      (in.token==NEWLINE
          //||       in.token==NEWLINES) {
            while (in.token==NEWLINE
               ||  in.token==NEWLINES) in.nextToken()
               
            val linePos = in.offset - in.lineStartOffset
            if (linePos <= in.linePosOfScriptsSection) mustExit = true
//if (mustExit) println(s"scriptDefsOrDcls mustExit linePos: $linePos linePosOfScriptsSection: ${in.linePosOfScriptsSection} token=${in.token}")            
          //}
        }
	      var name: Name = null
        var nameOffset = -1
		  
	      if (!mustExit) {
		      nameOffset = in.offset
		      name = ident()
		      if (name.toTermName == nme.ERROR) {mustExit = true
//println(s"scriptDefsOrDcls mustExit ident? name=$name ERROR")  
		      }
	      }
		      
	      if (!mustExit) {
//println(s"scriptDefsOrDcls scriptDef")            
		      val scriptDef = atPos(start, if (name.toTermName == nme.ERROR) start else nameOffset) {
		        
		        val Flags_SCRIPT = Flags.CASEACCESSOR // TBD
		        var newmods = mods | Flags_SCRIPT
		        // contextBoundBuf is for context bounded type parameters of the form
		        // [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
		        // i.e. (B[T] or T => B)
		        val contextBoundBuf = new ListBuffer[Tree]
		        val tparams         = typeParamClauseOpt(name, contextBoundBuf)
		        val vparamss        =     paramClauses  (name, contextBoundBuf.toList, ofCaseClass = false)
                val resultType      = typedOpt()
		        
		        newLineOptWhenFollowedBy(EQUALS)
		        //var restype = fromWithinReturnType(typedOpt()) // TBD: support Script return values
		        
		        
		        val rhs: Option[Tree] =
		          if (isStatSep || in.token == RBRACE || in.token != EQUALS) {
		            //if (restype.isEmpty) restype = scalaUnitConstr
		            newmods |= Flags.DEFERRED
		            None
		          } else Some {
		            in.isInSubScript_header = false
		            in.linePosOfScriptEqualsSym = in.offset - in.lineStartOffset
		            in.nextToken()
		            scriptExpression()
		          }
		        
		        
		        // so far for the parsing.
		        // now the parsed script is turned into a method.
		        // this part should be brought to a later compiler phase, before typer
		        
                // insert the _script(this, 'vkey, _k~??'k) part
		        // the parameter bindings such as _k~??'k are a little complicated
		        
		        val underscored_param_defs  = if (vparamss.isEmpty) Nil
		            else {
		              if (!vparamss.tail.isEmpty) syntaxError(nameOffset, "script should have only 1 parameter list")
		              vparamss.head map {p => 
		                
		                val possibly_underscored_p_name = 
		                  if (isFormalConstrainedParameter(p) 
		                  ||       isFormalOutputParameter(p)) newTermName(underscore_prefix(p.name.toString))
		                  else p.name // no underscore for formal input parameters

		                var tpt = 
		                  if (isFormalConstrainedParameter(p)) makeFormalConstrainedParameter(p.tpt)
		                  else if (isFormalOutputParameter(p)) makeFormalOutputParameter     (p.tpt)
		                  else                           p.tpt
		                
		                makeParam(possibly_underscored_p_name, tpt)
		              }
		            }
		        
		        val paramBindings           = if (vparamss.isEmpty) Nil
		            else {
		              if (!vparamss.tail.isEmpty) syntaxError(nameOffset, "script should have only 1 parameter list")
		              vparamss.head filter{p => isFormalConstrainedParameter(p) || isFormalOutputParameter(p)} map {p => 
		                
		                val pSym = Apply(scalaDot(nme.Symbol), List(Literal(Constant(p.name.toString)))) // TBD: ensure there is only 1 parameter list
		                val underscored_p_name = newTermName(underscore_prefix(p.name.toString))

		                var bindParam_Name = 
		                  if (isFormalConstrainedParameter(p)) bind_constrainedParam_Name
		                  else  /*isOutputParameter(p)*/         bind_outParam_Name
		                
		                val select = Select(Ident(underscored_p_name), bindParam_Name)
		                
		                Apply(select, List(pSym))
		              }
		            }
		        
		        // TBD: transform the tree for the accepted  `val i = @{x}: y` into the pseudo syntax `@{x}: val i = y`
		        
		        // If the script definition is not abstract, wrap the
		        // right hand side into the script header
		        // otherwise use EmptyTree
	          val scriptHeaderAndLocalsAndBody =
	            rhs map {
	              makeScriptHeaderAndLocalsAndBody(name.toString, _, paramBindings, resultType)
	            } getOrElse EmptyTree
	            
		        val underscored_script_name      = newTermName(underscore_prefix(   name.toString))

	            // to enable moving this all to a later phase, we should create a ScriptDef rather than a DefDef
	            DefDef(newmods, underscored_script_name, tparams, List(underscored_param_defs), scriptNodeType_resultType(resultType), scriptHeaderAndLocalsAndBody)
		      }
	        signalParseProgress(scriptDef.pos)
//println(s"scriptDefsOrDcls token=${in.token} scriptDef=$scriptDef")            

	        result+=scriptDef
	      }
	      scriptFormalOutputParameters     .clear
	      scriptFormalConstrainedParameters.clear
	      scriptLocalVariables             .clear
	      scriptLocalValues                .clear
	                                                             
	      if (!doMultipleScripts) mustExit = true
      }
//println(s"scriptDefsOrDcls exit")            
      
      in.isInSubScript_script = false
      in.isInSubScript_header = false
      result.toList
    }
    
    /*
  scriptExpression        = operatorModifiers scriptExpr_dataflow_lowPriority
 
  operatorModifiers       = . ("," + naryOperatorDesignator) . naryOperatorDesignator
 
  scriptExpr_dataflow_lowPriority = scriptExpr_lines (..; "~~>" scriptLambda . "+~/~>" scriptLambda + "~/~>" scriptLambda)  
  
  scriptExpr_lines        = scriptExpr_semicolons .. if newLineSignificant newLine else (-)
  scriptExpr_semicolons   = scriptExpr_if_do .. (+ ";"  ";-;"   ";+;")
  scriptExpr_if_do        = "if" valueExpression       "then" scriptExpr_if_do
                                                     . "else" scriptExpr_if_do
                          + "do" scriptExpr_dataflow ( "then" scriptExpr_if_do
                                                    %; "else" scriptExpr_if_do )
                          + scriptExpr_dataflow
  scriptExpression_6      = scriptExpression_5 .. (+ "||"  "|"
                                                     orParArrow
                                                     "|+"  "|;"  "|/"
                                                     "||+" "||;" "||/"
                                                     "|+|" "|;|" "|/|")
  scriptExpression_5      = scriptExpression_4 .. (+ "&&"  "&" andParArrow)
  scriptExpression_4      = scriptExpression_3 ..   "=="
  scriptExpression_3      = scriptExpression_2 ..    "+"
  scriptExpression_2      = scriptExpression_1 .. (+ "/"  "%"  "%/"  "%/%/"  "%&"  "%;")
  scriptExpression_1      = scriptSpaceExpression      ..    "·"
  scriptSpaceExpression   = scriptExpr_dataflow_highPriority     .. if commasOmittable (-) else (+)
  scriptExpr_dataflow_highPriority = scriptTerm (..; "~~>" scriptLambda . "+~/~>" scriptLambda + "~/~>" scriptLambda) . "^"
  
  scriptLambda            = . parameter "==>"; scriptTerm
  
  
  Note: FTTB ;-; and ;+; are not parsed (sequence operators ;-; for failure and ;+; for either success or failure
    Parsing these would require recognize these in the scanner, and treat them similarly to ; in several places
      */
    
    // TBD: clarify allowParameterList
    // TBD: alter dataflow expressions
    def scriptExpression(allowParameterList: Boolean = false): Tree = {
      
      var polishOp1: Name = SEMI_Name  //  first operator in prefix position, e.g. in (+ a b c) instead of (a+b+c)
      var polishOp2: Name = SEMI_Name  // second operator in prefix position, e.g. in (;+ a b c <NEWLINE> d e) instead of   (a; b; c) + (d; e)
	    var areSpacesCommas = false

      if (in.token == COMMA ) {areSpacesCommas = true; in.nextToken()}
      else {
        if (isSubScriptInfixOp(in)) {polishOp1 = in.name; in.nextToken()}
        else if (in.token==SEMI) {in.nextToken()}
      }
      if (isSubScriptInfixOp(in)) {polishOp2 = in.name; in.nextToken()}
      else if (in.token==SEMI) {in.nextToken()}
      else {polishOp2 = polishOp1}

      newLinesOpt()
      
      // note: the main call of scriptExpression, to scriptExpr_dataflow, 
      // is at the bottom of this method, after several local methods
    
      // scriptExpr_dataflow has two sets of operator priorities: 
      //
      // high priority - small arrow
      // x ~~> y
      // x ~~> y +~/~>z
      // x ~/~> y
      //
      // x ~(p)~> y
      // x ~(p1)~> y1 +~(p2)~> y2 +~/(e1)~>z1
      // x ~/(e)~> y1 +~/(e2)~>y2
      //
      // low priority - large arrows
      // x ~~~> y
      // x ~~~> y +~/~~>z
      // x ~/~~> y
      //
      // x ~~(p)~~> y
      // x ~~(p1)~~> y1 +~~(p2)~~> y2 +~/~(e1)~~>z1
      // x ~/~(e)~~> y1 +~/~(e2)~~>y2
      
	    def scriptExpr_dataflow(highPriority: Boolean, allowParameterList: Boolean = true): Tree = {
	      val TOKEN_BEGIN_NORMAL = if (highPriority) CURLYARROW2            else CURLYARROW3
	      val TOKEN_BEGIN_BROKEN = if (highPriority) CURLYBROKENARROW2      else CURLYBROKENARROW3
	      val TOKEN_PLUS_BROKEN  = if (highPriority) PLUS_CURLYBROKENARROW2 else PLUS_CURLYBROKENARROW3
	      
	      val TOKEN_BEGIN_NORMAL_PARAM = if (highPriority) CURLY2            else CURLY3
	      val TOKEN_BEGIN_BROKEN_PARAM = if (highPriority) CURLYBROKEN2      else CURLYBROKEN3
	      val TOKEN_PLUS_BROKEN_PARAM  = if (highPriority) PLUS_CURLYBROKEN2 else PLUS_CURLYBROKENARROW3
	      val TOKEN_PLUS_NORMAL_PARAM  = if (highPriority) PLUS_CURLY2       else PLUS_CURLY3
	      val TOKEN_END_PARAM          = if (highPriority) CURLYARROW2       else CURLYARROW3
	      
	      // parse expressions such as 
	      // ~~(p1 if g1)~~> y1 +~~(p2 ig g2)~~> y2 +~~(p3 if g3)~~> y3
	      // continue as long as the tokenToRepeat is available (e.g. "+~~")
	      // return a list of caseDefs pi,gi,yi 
	      // as always the guards gi are optional
	      //
	      // eat the first token; assuming it is an appropriate arrow.
	      // this may be the tokenToRepeat (starting with the plus), but it may also
	      // be first token of the compound arrow operator (e.g., "~~")
	      def parseArrowWithScriptLiteral_asMatch(tokenToRepeat: Int): Tree = {
	        def parseArrowWithScriptLiteral(): List[CaseDef] = {
						in.nextToken()
						val pos = accept(LPAREN) 
						//in.start_SubScript_partialScript 
						val p = pattern()
						val g = guard()
						accept(RPAREN)
						accept(TOKEN_END_PARAM)
						//in.start_SubScript_partialScript_caseScript 
						val sl = scriptLiteral(doInBrackets=false, simpleTermOnly=highPriority)
						//in.end_SubScript_partialScript_caseScript
						//in.end_SubScript_partialScript 
						val currentCase = atPos(pos){
						  makeCaseDef(p, g, sl)
						}
            currentCase :: (if (in.token==tokenToRepeat) parseArrowWithScriptLiteral() else Nil) 
	        }
	        val type_Script_Any              = AppliedTypeTree(s_scriptNodeType        , List(Ident(any_TypeName)))
	        val type_Function_Any_Script_Any = AppliedTypeTree(Ident(function_TypeName), List(Ident(any_TypeName), type_Script_Any))
          atPos(in.offset) {
	          Typed(Match(EmptyTree, parseArrowWithScriptLiteral()), type_Function_Any_Script_Any)
	        }
	      }
	      
	      var result = if (highPriority) scriptTerm(allowParameterList=allowParameterList) 
	                   else              scriptExpr_lines()
	                   
	      if (!isDefinitelyAFormalParameterList(result)) {
			    val pos = in.offset
		      in.token match {
		      	case TOKEN_BEGIN_NORMAL =>
			        val sourcePart = makeScriptHeaderAndLocalsAndBody("~~>", result, Nil, TypeTree())
			        in.nextToken()
			        val thenPart=scriptLambdaTerm(highPriority) 
			        if (in.token==TOKEN_PLUS_BROKEN) {
			          in.nextToken(); 
			          val elsePart=scriptLambdaTerm(highPriority)
			          result = atPos(pos) {Apply(subScriptDSLFunForDataflow_then_else, List(sourcePart, thenPart, elsePart))}
			        }
			        else result = atPos(pos) {Apply(subScriptDSLFunForDataflow_then     , List(sourcePart, thenPart))}
			        
		      	case TOKEN_BEGIN_BROKEN =>
			        val sourcePart = makeScriptHeaderAndLocalsAndBody("~/~>", result, Nil, TypeTree())
	            in.nextToken(); 
			        val elsePart=scriptLambdaTerm(highPriority)
	            result = atPos(pos) {Apply(subScriptDSLFunForDataflow_else, List(sourcePart, elsePart))}
			        
		      	case TOKEN_BEGIN_NORMAL_PARAM => 
			        val sourcePart = makeScriptHeaderAndLocalsAndBody("~~>", result, Nil, TypeTree())
		      	  val normalScriptCaseDefs = parseArrowWithScriptLiteral_asMatch(TOKEN_PLUS_NORMAL_PARAM)
		      	  if (in.token==TOKEN_PLUS_BROKEN_PARAM) {
		      	    val brokenScriptCaseDefs = parseArrowWithScriptLiteral_asMatch(TOKEN_PLUS_BROKEN_PARAM)
			          result = atPos(pos) {Apply(subScriptDSLFunForDataflow_then_else, List(sourcePart, normalScriptCaseDefs, brokenScriptCaseDefs))}
		      	  }
		      	  else {
			          result = atPos(pos) {Apply(subScriptDSLFunForDataflow_then, List(sourcePart, normalScriptCaseDefs))}
		      	  }

		      	case TOKEN_BEGIN_BROKEN_PARAM => 
			        val sourcePart = makeScriptHeaderAndLocalsAndBody("~/~>", result, Nil, TypeTree())
              val brokenScriptCaseDefs = parseArrowWithScriptLiteral_asMatch(TOKEN_PLUS_BROKEN_PARAM)
			        result = atPos(pos) {Apply(subScriptDSLFunForDataflow_else, List(sourcePart, brokenScriptCaseDefs))}
              
		      	case _ =>
		      }
	      }
	      result
	    }
	    
      def scriptExpr_lines(): Tree = {
        def hasMoreOperands(): Boolean = {
          if (in.token!=NEWLINE) return false
          in.nextToken()
          val isLeftOfEqualsSym = in.isLeftOfEqualsSym(in.offset)
//println(s"hasMoreOperands token=${in.token} name=${in.name} isLeftOfEqualsSym=${isLeftOfEqualsSym}")
          in.scriptExpressionParenthesesNestingLevel > 0 || !isLeftOfEqualsSym
        }
        val operand = scriptExpr_semicolons()
        
        if (!hasMoreOperands()) return operand
        
        val pos = in.offset        
        var operands = List[Tree](operand)
        do {
          operands = operands:::List(scriptExpr_semicolons())
        } while (hasMoreOperands())
//println(s"scriptExpr_lines operands=$operands token=${in.token}")
        atPos(pos) {
	        Apply(subScriptDSLFunForOperator(Ident(polishOp2), spaceOp=null, newlineOp=null), operands)
	      }
      }
      def scriptExpr_semicolons(): Tree = {
        def hasMoreOperands = in.token==SEMI
        val operand = scriptExpr_if_do()
        if (!hasMoreOperands) return operand
        
        val pos = in.offset        
        var operands = List[Tree](operand)
        do {
          in.nextToken()
          operands = operands:::List(scriptExpr_if_do())
        } while (hasMoreOperands)
//println(s"scriptExpr_semicolons operands=$operands")

        atPos(pos) {
	        Apply(subScriptDSLFunForOperator(Ident(SEMI_Name), spaceOp=null, newlineOp=null), operands)
	      }
      }
      def scriptExpr_if_do(): Tree = {
        val currentToken = in.token
        currentToken match {
	        case IF => 
            def parseIf = { 
		          in.isInSubScript_nativeCode=true
		          atPos(in.skipToken()) {
		            val startPos = r2p(in.offset, in.offset, in.lastOffset max in.offset)
		            val cond  = expr()
		            in.isInSubScript_nativeCode=false
		            accept(THEN)
		            val thenp = scriptExpr_if_do()
		            if (in.token == ELSE) {
		                 in.nextToken()
		                 val elsep = scriptExpr_if_do()
		                 Apply(Apply(dslFunFor(ELSE), List(blockToFunction_here(cond, vmNodeFor(ELSE), startPos))),List(thenp, elsep))
		            }
		            else Apply(Apply(dslFunFor(  IF), List(blockToFunction_here(cond, vmNodeFor(  IF), startPos))),List(thenp))
		          }
		        }
		        parseIf
		      case DO => 
		        def parseDo = atPos(in.skipToken()) {
		          val startPos = r2p(in.offset, in.offset, in.lastOffset max in.offset)
		          val doPart  = scriptExpr_if_do()
		          if (in.token==THEN) {
		            accept(THEN)
		            val thenp = scriptExpr_if_do()
		            if (in.token == ELSE) {accept(ELSE); val elsep = scriptExpr_if_do()
		                 Apply(dslFunFor(DO_THEN_ELSE), List(doPart, thenp, elsep))
		            }
		            else Apply(dslFunFor(DO_THEN),List(doPart, thenp))
		          }
		          else {                   accept(ELSE); val elsep = scriptExpr_if_do()
		                 Apply(dslFunFor(DO_ELSE), List(doPart, elsep))
		          }
		        }
		        parseDo
		      case _ => scriptExpression_6()	        
	      }
	    }
      
	    // expressions with operators such as + & && | || /
	    def scriptExpression_6(): Tree = {
	 
	      case class ScriptOpInfo(operand: Tree, operatorName: Name, offset: Offset, length: Int)
	    
	      var scriptOperatorStack: List[ScriptOpInfo] = Nil
	      val base = scriptOperatorStack
	      var top: Tree = null
	      
		    def replaceOperatorsByFunctions(top: Tree, spaceOp: Name, newlineOp: Name): Tree = {
		      top match {
		
		        case Apply(fun: Tree, args: List[Tree])  => 
		          val f = if (isSubScriptOperator(fun)) subScriptDSLFunForOperator(fun, spaceOp, newlineOp) else fun
		          Apply(f, args.map(replaceOperatorsByFunctions(_, spaceOp, newlineOp)))
		          
		        case _            => stripParens(top)
		      }
		      
		    }
	      def reduceScriptOperatorStack(prec: Int): Unit = {
	        while (scriptOperatorStack != base && prec <= subScriptInfixOpPrecedence(scriptOperatorStack.head.operatorName)) {
	          val opinfo          = scriptOperatorStack.head
	          scriptOperatorStack = scriptOperatorStack.tail
	          
	          //val start  = opinfo.operand.pos.startOrPoint
	          //val end    =            top.pos.  endOrPoint
	          val opPos = r2p(opinfo.offset, opinfo.offset, opinfo.offset+opinfo.operatorName.length); 
	          val lPos  = opinfo.operand.pos
	          val rPos  = top.pos
	          val start = if (lPos.isDefined) lPos.startOrPoint else opPos.startOrPoint;               
	          val end   = if (rPos.isDefined) rPos.  endOrPoint else opPos.endOrPoint
	          
	          val newArgsBut1 = opinfo.operand match {
	            case apply @ Apply(Ident(n), args: List[Tree]) 
	                                 if n==opinfo.operatorName => args // ??? also affect start and end ???
	            case _                                         => List(opinfo.operand)
	          }
	          
	          val OP_Ident = Ident(opinfo.operatorName)
	          top = atPos(start, opinfo.offset, end) {Apply(OP_Ident, newArgsBut1:::List(top))} // TBD: set positions better
	        }
	      }
	
	      var savedAreSpacesCommas = areSpacesCommas
	
	      areSpacesCommas = false
	      // prefix ops, e.g. "(;+ a b \n c d \n e f)" is shorthand for "a b + c d + e f"
	      
	      var moreTerms = true
	      var isFirst = true
	      do {
	        top = scriptSpaceExpression(allowParameterList && isFirst)
	        isFirst = false
	        if (isDefinitelyAFormalParameterList(top)) {
	          moreTerms = false
	        }
	        else if (isSubScriptInfixOp(in)) {
	           val opName = in.name
	           reduceScriptOperatorStack(subScriptInfixOpPrecedence(opName))
	           scriptOperatorStack = ScriptOpInfo(top, opName, in.offset, in.name.length) :: scriptOperatorStack // TBD: new line
	           in.nextToken() // eat the operator
	        }
	        //else if (in.isSubScriptTermStarter(in) && in.afterLineEnd()) {
	           // TBD: probably this should be removed, but then more NEWLINEs should be recognized by the scanner, for subScript expressions
	       //    reduceScriptOperatorStack(subScriptInfixOpPrecedence(NEWLINE_Name))
	       //    scriptOperatorStack = ScriptOpInfo(top, NEWLINE_Name, in.offset, 1 /*in.name.length*/) :: scriptOperatorStack // TBD: new line
	       // }
	        else moreTerms = false
	      } while (moreTerms)
		  reduceScriptOperatorStack(0)
	
		  val result = replaceOperatorsByFunctions(top, spaceOp=polishOp1, newlineOp=polishOp2)
	
	//println("Result tree")
	//println("-------------------")
	//println(result)
	//println("===================")
		  
	      areSpacesCommas              = savedAreSpacesCommas
	      
	      result
	    }
	    
    
    def scriptLambdaTerm(highPriority: Boolean): Tree = {
      // TBD: support USCORE?
      var result = simpleScriptTerm(allowParameterList=true) // TBD: allow that this returns a single-parameter list, e.g. (i:Int)
      val parameterList = result match {
            case ident @ Ident(_) if in.token==ARROW2 => List(convertToParam(ident))
            case typed @ Typed(p,aType)               => List(convertToParam(typed))
            case _ => null
      }
      if (parameterList!=null) {
        val pos = accept(ARROW2)
        //val lambda = scriptExpr_dataflow(highPriority)
        val lambda = scriptLiteral(doInBrackets=false, simpleTermOnly=highPriority)
        result = atPos(pos) {Function(parameterList, lambda)}
      }
      result
    }
    def scriptSpaceExpression(allowParameterList: Boolean): Tree = {
      val ts  = new ListBuffer[Tree]
      var isFirst = true
      var moreTerms = true
      do  {
        var p = scriptExpr_dataflow(highPriority=true, allowParameterList = allowParameterList && isFirst)
        //val p = scriptTerm(allowParameterList && isFirst)
        
        if (in.token==CARET) {
            p = atPos(in.offset) {ScriptUserElement(Caret_Name, p, null, null)}
            in.nextToken()
        }
        
        ts += p

        if (areSpacesCommas 
        || !in.isSubScriptTermStarter
        ||  isDefinitelyAFormalParameterList(p)) {
          moreTerms = false  // TBD: check newLinesAreSpecialSeparators
        }
      }
      while (moreTerms)
        
      if (ts.length == 1)  ts.head
      else Apply(SPACE_Ident, ts.toList)
    }

    def scriptCommaExpression(allowParameterList: Boolean, isNegated: Boolean): Tree = {
      val oldOffset = in.offset
      val ts  = new ListBuffer[Tree]
      var moreTerms = true
      var isFirst = true
      do  {
        val t = simpleScriptTerm(allowParameterList = allowParameterList && isFirst, isNegated)
        ts += t
        isFirst = false
        if (isDefinitelyAFormalParameterList(t)) {
          moreTerms = false
        }
        else if   (in.token==COMMA) {in.nextToken()/*; eatNewlines()*/}
        else if (!areSpacesCommas 
             ||  !in.isSubScriptTermStarter) moreTerms = false  // TBD: check newLinesAreSpecialSeparators
      }
      while (moreTerms)
        
      val allTermsArePathsOrLiterals: Boolean = {
          ts.forall(t =>
            t match {
              case Select(Select(_,n: TermName), _) if n.toString==nameDSL.toString => false
              case Select(_,_)                      => true 
              case Ident(_) | Literal(_) => true 
           //case Select(_,_) | Ident(_) | Literal(_) => true 
              case _ => false}
          )
      }
      if (allTermsArePathsOrLiterals) atPos(ts.head.pos.startOrPoint) {ScriptApply(EmptyTree, ts.toList, false)}
      else if (ts.length == 1)  ts.head
      else {syntaxError(oldOffset, "terms in comma expression should be path or literal"); ts.head}
    }

    def scriptTerm(allowParameterList: Boolean): Tree = (in.token: @scala.annotation.switch) match {
      case VAL     => scriptLocalValOrVar((NoMods                ) withPosition(VAL , tokenRange(in)))
      case VAR     => scriptLocalValOrVar((NoMods | Flags.MUTABLE) withPosition(VAR , tokenRange(in)))
      case PRIVATE => ??? // TBD
      case _       => postfixScriptTerm(allowParameterList)
    }
    
    def postfixScriptTerm (allowParameterList: Boolean): Tree = {
      var result = unaryPrefixScriptTerm(allowParameterList)
      if (!isDefinitelyAFormalParameterList(result)) {
        while (isSubScriptPostfixOp(in)) {
          result = atPos(in.offset) {
            val name = nme.toUnaryName(rawIdent().toTermName)
            in.nextToken()
            Select(stripParens(result), name)
          }
        }
      }
      result
    }

    def unaryPrefixScriptTerm (allowParameterList: Boolean): Tree = 
      if (isSubScriptUnaryPrefixOp(in)) {
        val oldOffset = in.offset
        val name = in.name
        //val name = nme.toUnaryName(rawIdent().toTermName)    ????
        in.nextToken
        atPos(in.offset) {
          if (name == nme.UNARY_- && isNumericLit && in.offset==oldOffset+1) {
            scriptCommaExpression(allowParameterList = false, isNegated = true)
          }
          else {
            Select(stripParens(unaryPrefixScriptTerm(allowParameterList = false)), name)
          }
        }
      }
      else (in.token: @scala.annotation.switch) match {
      case AT => parseAnnotationScriptTerm
      case _ => scriptCommaExpression(allowParameterList, isNegated = false)
    }

    // apply wildcard parameter to given type, 
    // i.e. for T return T[_]
    // much copied from def placeholderTypeBoundary
    def applyWildcardParameterToType(tree: Tree, startPos: Int): Tree = {
      val savedPlaceholderTypes = placeholderTypes
      placeholderTypes = List()
      val wct = wildcardType(startPos) // adds to placeholderTypes
      val at = atPos(tree.pos){AppliedTypeTree(tree, List(wct))}
      val et = atPos(tree.pos) { ExistentialTypeTree(at, placeholderTypes.reverse) }
      placeholderTypes = savedPlaceholderTypes
      et
    }
    
    
    
    // for input: @{annotationCode}: body     
    // generate : DSL._at( (here:N_annotation[CN,CT])=>{implicit val there=here.there; annotationCode}) {body}
    // in DSL:
    //
    // def _at[N<:CallGraphNode,T<:Child](_cf:N_annotation[N,T]=>Unit)  
    //   = (_child: T) => T_annotation[N,T]((here:N_annotation[N,T]) => _cf(here), _child)
    //
    // activation: case n@N_annotation(t  ) => activateFrom(n, t.child0); executeCode(n)
    // createNode: case t@T_annotation(_,_) => N_annotation(t)


    def parseAnnotationScriptTerm: Tree = {
      atPos(in.token) {
        val offset = in.offset
        val startPos = r2p(in.offset, in.offset, in.lastOffset max in.offset)
        val annotationCode = parseAnnotation
        val body           = stripParens(unaryPrefixScriptTerm(allowParameterList = false))

        
        val vmNodeTypeOfBody = body match { 
          case ScriptCodeFragment(token, code) => atPos(startPos){AppliedTypeTree(vmNodeFor(token), List(Ident(any_TypeName)))}
          case Apply(fun, Function(List(ValDef(_,_, nodeType,_)),block)::_) => nodeType
          case Apply(Select(_, fun_name), _) => vmNodeFor(LPAREN_ASTERISK2) // brutally assuming that fun_name is _launch_anchor
          case ScriptApply(_,_,_) => vmNodeForCall_Any
          case _ => Ident(any_TypeName) // TBD
        }
      
        
        // OLD:
        // val applyAnnotationCode = Apply(dslFunFor(AT), List(blockToFunction_there(annotationCode, vmNodeTypeOfBody, startPos)))
        // Apply(applyAnnotationCode, List(body))

        val termName_N_annotation = newTermName("N_annotation") // must be term name! 
        val typeName_N_annotation = newTypeName("N_annotation")  
                                 // see case class TypeApply(fun: Tree, args: List[Tree])... assert(fun.isTerm, fun)
        val vmnodeType_N_annotation = Select(sSubScriptVM, typeName_N_annotation)

        // Generate: DSL._at{here: N_annotation[N_..., T_...] => implicit val there = here.there; annotationCode}{body}
        
        // very messy: conversion util node type into template type
        def templateTypeNameForNodeTypeName(tn: Name) = newTypeName("T"+tn.toString.substring(1))
        def templateTypeNameForNodeType(n: Tree) = n match {case Select(_,name) => templateTypeNameForNodeTypeName(name)}
        def templateTypePathForNodeType(n: Tree) = Select(sSubScriptVMModelTemplateConcrete, templateTypeNameForNodeType(n))
        
        def templateTypePathForToken(token: Int) = Select(sSubScriptVMModelTemplateConcrete, mapTokenToVMTemplateTypeName(token))

        val templateOfBodyType = body match {
          case Apply(fun, Function(List(ValDef(_,_, nodeType,_)),block)::_) => templateTypePathForNodeType(nodeType)
          case Apply(Select(_, fun_name), _) => templateTypePathForToken(LPAREN_ASTERISK2) // brutally assuming that fun_name is _launch_anchor
          case ScriptCodeFragment(token, code) => atPos(startPos){AppliedTypeTree(templateTypePathForToken(token), List(Ident(any_TypeName)))}
                                // applyWildcardParameterToType(templateTypePathForCodeFragmentToken(token), offset)
                                //val str = s"T_${mapTokenToVMNodeString(token)}"
                                //q"subscript.vm.template.concrete.$str"
          case anything => anything // println(s"templateOfBodyType unmatched: $vmNodeTypeOfBody")
        }
        
        
        
        val typeTree = AppliedTypeTree(vmnodeType_N_annotation, List(vmNodeTypeOfBody, templateOfBodyType))
        
        val lambda_here_annotationCode = { // elsewhere similar things are done in "blockToFunction"
          
          val vparams = List(
            atPos(startPos) {
              makeParam(here_Name, typeTree setPos startPos)
            }
          )
          val implicitVal                    = atPos(startPos) {ValDef(Modifiers(Flags.IMPLICIT), there_Name, TypeTree(),  Select(here_Ident, there_Name))}
          val implicitVal_seq_annotationCode = makeBlock(List(implicitVal, annotationCode))
       
          Function(vparams, implicitVal_seq_annotationCode)
        }
        val applyAnnotationCode = Apply(dslFunFor(AT), List(lambda_here_annotationCode))
        
        Apply(applyAnnotationCode, List(body))
      }
    }
    
    // at last, the body and end of method scriptExpression:
    
    var result = scriptExpr_dataflow(highPriority=false)
    if (in.token==CARET2) {
        result = atPos(in.offset) {ScriptUserElement(Caret_Name, result, null, null)}
        in.nextToken()
    }
    result
  }

  def isDefinitelyAFormalParameterList(p: Tree) = p match {
    case Typed(_,_) => true 
    case Ident(name) if name.toString() startsWith "x$" => true // result of USCORE after freshTermName
    case _ => false
  }

  def simpleNativeValueExpr(allowBraces: Boolean = false): Tree = {
    
    val ret = in.token match {
      case LBRACE if  allowBraces => in.isInSubScript_nativeCode=true; in.nextToken(); val r=block(); in.isInSubScript_nativeCode=false; accept(RBRACE); r
      case LPAREN if !allowBraces => in.isInSubScript_nativeCode=true; in.nextToken(); val r=expr (); in.isInSubScript_nativeCode=false; accept(RPAREN); r
      case IDENTIFIER 
      |    BACKQUOTED_IDENT 
      |    THIS 
      |    SUPER            => path(thisOK = true, typeOK = false)
      case _ if (isLiteral) => atPos(in.offset)(literal())
      case _ => {syntaxError(in.offset, "native value expresion expected"); EmptyTree}
    }
    // newLinesOpt() NO; newlines are special; old: NEWLINEs may have remained from the "native code mode" scanning
    ret
  }

    
/*
  scriptTerm              =+ postfixScriptTerm
                             variableDeclaration 
                                valueDeclaration 
                              privateDeclaration 
 
  valueDeclaration        = "val" identifier; . ":" typer ;   "=" simpleValueExpression
  variableDeclaration     = "var" identifier (  ":" typer ;%; "=" simpleValueExpression)
   privateDeclaration     = "private" identifiers

  postfixScriptTerm       = ..(unaryPrefixOperator + directive); scriptCommaExpression; . "^" . variableDesignator
                            
  scriptCommaExpression   = simpleScriptTerm .. ","
 
  directive               = "@" scalaCode ":"
 
  unaryPrefixOperator     =+ "!"  "-"  "~"
 */


    def scriptLocalValOrVar(mods: Modifiers): Tree = {
      val pos = in.offset
      var newmods = mods
      in.nextToken()
      val tok  = in.token
      val name = ident()
      val lhs  = atPos(pos) {
          if (tok == BACKQUOTED_IDENT) Ident(name) updateAttachment BackquotedIdentifierAttachment
          else Ident(name)
      }
      
      // A pattern function that captures the tree that will be constructed
      // Also, it captures the fact of passing rhs or tp out of the scope
      // in order to generate definitions at the beginning of the script
      //
      // isTypeTaransmittable variable indicates whether `tp` type can be
      // transfered out of current scope (true) or not (false)
      def operationPattern(rhs: Tree, tp: Tree, isTypeTransmittable: Boolean) = {
        import TypeOperations._
        
        val vIdent          = Ident(newTermName(underscore_prefix(name.toString)))
        val  sFunValOrVar   = dslFunFor(if (mods.isMutable) VAR else VAL)
        val sNodeValOrVar   = vmNodeFor(if (mods.isMutable) VAR else VAL)
     
        val typer           = AppliedTypeTree(sNodeValOrVar, List(tp))
       
        // If the type is transmittable (already known), enforce it
        // If it is just an unknown identifier, infer it
        val initializerCode =
          if (isTypeTransmittable) blockToFunction_here (enforcingType(tp)(rhs), typer, rhs.pos)
          else withTypeOf(rhs, tp.asInstanceOf[Ident]){_ =>
            blockToFunction_here(rhs, typer, rhs.pos)  // Typer is already influenced by `tp` type parameter
          }
        
        // If tp is just a local identifier, it will be useless out of this scope
        // Hence, we'll need to pass the actual value `rhs` instead of `tp`
        // in order to infer the type from it on later stages of compilation
        // (see TypeOperations.withTypeOf transformation)
        //
        // Also, we'll need to set a proper Boolean flag in order to distinguish
        // between values and types trees
        
        val pairToAdd = name->(if (isTypeTransmittable) tp else rhs, isTypeTransmittable)
        if (mods.isMutable) scriptLocalVariables += pairToAdd
        else                scriptLocalValues    += pairToAdd
        
        atPos(pos) {
          if (rhs.isEmpty) {dslFunFor(LPAREN_PLUS_MINUS_RPAREN)} // neutral; there is no value to provide
          else  Apply(sFunValOrVar, List(vIdent, initializerCode))
        }
      }
      
      in.token match {
        // Type is present
        case COLON =>
          accept(COLON)
          val tp  = exprSimpleType()
          val rhs =
            if (tp.isEmpty || in.token == EQUALS || !newmods.isMutable || true /*FTTB enforce initialisation*/) {
              accept(EQUALS)
               
              val annotation = if (in.token==AT) parseAnnotation else null
              val ex = if (!tp.isEmpty && newmods.isMutable &&
                           lhs.isInstanceOf[Ident] && in.token == USCORE) {
                           in.nextToken()
                           newmods = newmods | Flags.DEFAULTINIT; EmptyTree}
                       else {
                         in.start_SubScript_val_var_init; 
                         try expr() 
                         finally in.end_SubScript_val_var_init 
                       }
              if (annotation==null) ex else ex // TBD: make something using annotation
            }
            else {newmods = newmods | Flags.DEFERRED; EmptyTree}
          
          operationPattern(rhs, tp, true)
          
          // TBD: val result = ScriptValDef(newmods, name.toTermName, tp, rhs)    FTTB a quick solution:
          // val c = initializer ===> subscript.DSL._val(_c, here: subscript.DSL.N_localvar[Char] => initializer)   likewise for var
      
        // Type is absent
        // Copy pasting is not good - further abstractin will be required
        // for `rhs` computation
        case _ =>
          if (true) {
            syntaxError(in.offset, "For the time being local script vars and vals must be explicitly typed"); EmptyTree
          }
          else {
            val rhs = {
              accept(EQUALS)
              val annotation = if (in.token==AT) parseAnnotation else null
                
              val ex = if (newmods.isMutable &&
                           lhs.isInstanceOf[Ident] && in.token == USCORE) {
                           in.nextToken()
                           newmods = newmods | Flags.DEFAULTINIT; EmptyTree}
                       else {in.start_SubScript_val_var_init; try expr() finally in.end_SubScript_val_var_init}
              if (annotation==null) ex else ex // TBD: make something using annotation
            }
            operationPattern(rhs, Ident(newTypeName("T")), false)
          }
      }

      
    }


    def parseAnnotation: Tree = {
      atPos(in.skipToken()) {
        val annotationCode = simpleNativeValueExpr(allowBraces = true); accept(COLON)
        annotationCode
      }
    }
    
    /*
  actualParameterList     = "(" (.actualParameters) ")"
 
  identifiers             =       identifier      .. ","
  formalParameters        =       formalParameter .. ","
  actualParameters        =       actualParameter .. parameterSeparator
  simpleActualParameters  = simpleActualParameter .. parameterSeparator
 
  formalParameter         = . formalOutputMarker; identifier ":" typer
  actualParameter         = valueExpression
                          + actualOutputMarker valueExpression
                            (.postCondition) (. ":" type)
  simpleActualParameter   = simpleValueExpression
                          + actualOutputMarker simpleValueExpression
                            (.postCondition) (. ":" type)
 
  formalOutputMarker      = "?" + "??"
  actualOutputMarker      = "?" + "??"
 
  postCondition           = "?if" valueExpression
 
  simpleTerm              =; |+|
                            scriptCall
                            codeFragment 
                            matchTerm
                            throwTerm
                            whileTerm
                            forTerm
                            tryTerm
                            specialTerm 
                            "("   scriptExpression   ")"
                            "(*"  scriptExpression  "*)"
                            "(**" scriptExpression "**)"
 
  scriptCall              = implicitScriptCall
                        |+| methodOrScriptCall
                        |+|  channelScriptCall; . resultHandler

  implicitScriptCall      =  simpleActualParameters
  methodOrScriptCall      =  simpleValueExpression  .  actualParameterList .postCondition
                        |+|  simpleValueExpression "," simpleActualParameters
  channelScriptCall       = .simpleValueExpression;
                             identifier_arrow;  (+) +  actualParameterList .postCondition
                                                    +  simpleActualParameters
 
  resultHandler           = "^" + "^^" "{" scalaCode "}"
  parameterSeparator      = "," + if commasOmittable (+)
 
  specialTerm             =+ "(-)"  "(+)"  "(+-)"   "."  ".."  "..."  "break"
 
 
  codeFragment            =; + 
                           "{"    scalaCode    "}"
                           "{*"   scalaCode   "*}"
                           "{?"   scalaCode   "?}"
                           "{!"   scalaCode   "!}"
                           "{^"   scalaCode   "^}"
                           "{."   scalaCode   ".}"
                           "{..." scalaCode "...}"
 
  whileTerm               = "while" valueExpression 
  throwTerm               = "throw" valueExpression 
  forTerm                 = "for"; "(" enumerators ")" + "{" enumerators "}"
  tryTerm                 = "try" unary (scriptCatchClause %; scriptFinallyClause)
  matchTerm               = simpleValueExpression "match" "(" scriptCaseClauses ")"
 
  scriptCatchClause       = "catch" "(" (scriptCaseClause..) ")"
 
  scriptCaseClause        = "case" pattern (. "if" valueExpression)
                            ("=>" + "*=>") scriptExpression
 
  scriptFinallyClause     = "finally" "{" scalaCode "}"
 
  valueExpression         = parenthesizedExpression + simpleValueExpression
 
  parenthesizedExpression = "(" scalaExpression ")"
 
     */
    def simpleScriptTerm (allowParameterList: Boolean, isNegated: Boolean = false): Tree = atPos(in.offset) {
      val currentToken = in.token
      currentToken match {
      case WHILE =>
        def parseWhile    = atPos(in.skipToken()) {
          val startPos    = r2p(in.offset, in.offset, in.lastOffset max in.offset)
          val lname: Name = freshTermName(nme.WHILE_PREFIX)
          val cond        = simpleNativeValueExpr()
          Apply(dslFunFor(WHILE), List(blockToFunction_here(cond, vmNodeFor(WHILE), startPos)))
        }
        parseWhile
      case LPAREN_PLUS_RPAREN           
         | LPAREN_MINUS_RPAREN              
         | LPAREN_PLUS_MINUS_RPAREN         
         | LPAREN_SEMI_RPAREN               
         | DOT                              
         | DOT2                             
         | DOT3                         => atPos(in.offset){in.nextToken(); dslFunFor(currentToken)} // TBD: transform in later phase
      case IDENTIFIER if (isBreakIdent) => atPos(in.offset){in.nextToken(); dslFunForBreak         }
      
      case LPAREN                       => atPos(in.offset){inScriptParens(currentToken, scriptExpression(allowParameterList))}
      case LPAREN_ASTERISK
         | LPAREN_ASTERISK2             => atPos(in.offset){inScriptParens(currentToken, scriptExpression())}
      case LBRACE           
         | LBRACE_DOT              
         | LBRACE_DOT3         
         | LBRACE_QMARK   
         | LBRACE_EMARK                              
         | LBRACE_ASTERISK                             
         | LBRACE_CARET                 => scriptBlockExpr()
        
      case USCORE if allowParameterList => var p = freshPlaceholder() // for script lambdas
                                           if (in.token==COLON) {
                                             in.nextToken()
                                             p = atPos(p.pos) {Typed(p, typ())}
                                           }
                                           p
        
      case IDENTIFIER if (in.name==nme.QMARKkw || in.name==nme.QMARK2kw) => 
        val isOutputParam   = in.name==nme.QMARKkw; 
        in.nextToken
        val p = path(thisOK = true, typeOK = false); // scriptSimpleExprRest(t, canApply = canApply)
	    var paramConstraint: Tree = null
        if (in.token==IF_QMARK) {
            in.nextToken
            paramConstraint = simpleNativeValueExpr()
        }
        if (isOutputParam) makeActualOutputParameter(p, paramConstraint)
        else             makeActualAdaptingParameter(p, paramConstraint)

      case LESS2 => in.nextToken(); 
                    in.start_SubScript_partialScript // instruct scanner to see ==> and >>
                    val result = makeMessageHandler(if (in.token==CASE) scriptMsgCaseClauses()
                                                    else           List(scriptMsgCaseClause()))
                    in.end_SubScript_partialScript
                    accept(GREATER2)
                    result

      case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER if (!isSubScriptInfixOp(in)) => 
        val p = path(thisOK = true, typeOK = false); // scriptSimpleExprRest(t, canApply = canApply)
        if (allowParameterList && in.token==COLON) { // for ScriptLambdas
          p match {case Ident(_) => in.nextToken(); return atPos(p.pos.startOrPoint, in.offset) {Typed(p, typ())} case _ =>}
        }
        if (in.token==LPAREN && in.offset==in.lastOffset) { // !!!! no space acceptable before "("
	      val arguments = scriptArgumentExprs()
          var callConstraint: Tree = null
          if (in.token==IF_QMARK) {
            in.nextToken
            callConstraint = simpleNativeValueExpr()
          }
	      atPos(p.pos.startOrPoint, in.offset) {ScriptApply(p, arguments, false)} // TBD: use callConstraint
        }
        else p
      case _ if (isLiteral) => atPos(in.offset)(literal(isNegated))
      case NEW    => syntaxError(in.offset, "'new' expressions not yet supported in script bodies"); EmptyTree
      case _      => syntaxErrorOrIncomplete(s"illegal start of simple script term: $currentToken", true); errorTermTree
    }
    }
    
    // Call a script r$(m)
    // with m = usual pf with each case appended: ([scriptExpr] if such an expr given, else null)
    //
    // r$ is a method (presumably in SubScriptActor) that accepts a pf and sets it up for msg reception:
    //
    //  def script r$(handler: PartialFunction[Any, Script[Any]])
    //  = var s:Script[Any]=null
    //    @{val handlerWithExecuteAA = handler andThen {hr => {s = hr; there.eventHappened}}
    //                          synchronized {callHandlers += handlerWithExecuteAA}
    //      there.onDeactivate {synchronized {callHandlers -= handlerWithExecuteAA}}
    //    }:
    //    {. Debug.info(s"$this.r$$") .}
    //    if s != null then s
    //
    // Note: we also do transformations for <<....>> here.
    // Probably different method required for general partial script closures...
    
    case class ScriptMsgCase(offset: Offset, pat:Tree, guard:Tree, scriptCaseBlockStatSeq: List[Tree], scriptCaseScript:Tree)
    
    def makeMessageHandler(scriptMsgCases: List[ScriptMsgCase]): Tree = {
      val offset        = scriptMsgCases.head.offset
      val caseClauses   = scriptMsgCases.map(makeMsgScriptCaseDef(_)) 
      val m             = Match(EmptyTree, caseClauses)
      
      atPos(offset) {ScriptApply(Ident(MsgSCRIPT_Name), List(m), false)} // r$(m)   
    }
        
    def makeMsgScriptCaseDef(smct:ScriptMsgCase): CaseDef = { 
      // append the script lambda to the normal case def; null if absent
      val scriptLambdaValue: Tree = if  (smct.scriptCaseScript==EmptyTree) newLiteral(null)
                                    else smct.scriptCaseScript
          
      val scriptCaseBlockStatSeq_maybe_with_sender = 
        
        if (smct.scriptCaseScript==EmptyTree) smct.scriptCaseBlockStatSeq                              
        else { // generate: val sender=this.sender
           val this_sender = atPos(smct.offset) {Select(atPos(smct.offset) {This(tpnme.EMPTY)}, sender_Name)}
           val val_Sender_assigned_this_Sender:Tree = atPos(smct.offset) {
             
             ValDef(NoMods, sender_Name, TypeTree(), this_sender)
           }
           val_Sender_assigned_this_Sender::smct.scriptCaseBlockStatSeq
        }
                                    
      val caseBlock = makeBlock(scriptCaseBlockStatSeq_maybe_with_sender :+ scriptLambdaValue)
      // construct the usual case def
      atPos(smct.offset){makeCaseDef(smct.pat, smct.guard, caseBlock)}
    }
    def scriptMsgCaseClause(): ScriptMsgCase = {
      new ScriptMsgCase(in.offset, pattern(), guard(), scriptCaseBlockStatSeq(), scriptCaseScript())
    }
    
    /** {{{
     *  CaseClauses ::= CaseClause {CaseClause}
     *  CaseClause  ::= case Pattern [Guard] `=>' Block
     *  }}}
     */
    def scriptMsgCaseClauses(): List[ScriptMsgCase] = caseSeparated { scriptMsgCaseClause() }

 // def scriptCaseBlock       ( ): Tree       = if (in.token==ARROW ) atPos(accept(ARROW ))(block()) else EmptyTree
    def scriptCaseBlockStatSeq( ): List[Tree] = if (in.token==ARROW ) {accept(ARROW ); blockStatSeq()} else Nil
    def scriptCaseScript(): Tree = if (in.token==ARROW2) {
                                      in.start_SubScript_partialScript_caseScript; 
                                      atPos(accept(ARROW2)){
                                        val sl = scriptLiteral(doInBrackets=false, simpleTermOnly=false)
                                        in.  end_SubScript_partialScript_caseScript
                                        sl
                                      }
                                   }
                                   else EmptyTree



    def scriptBlockExpr(): Tree = {
      in.isInSubScript_nativeCode = true
      val startBrace = in.token
      val   endBrace = scriptBracePairs(startBrace);
      val startPos   = r2p(in.offset, in.offset, in.lastOffset max in.offset)
      accept(startBrace)
      val code = block()
      val ret = atPos(startPos){ScriptCodeFragment(startBrace, code)}
      in.isInSubScript_nativeCode = false
      accept(endBrace)
      ret
    }
    
    def scriptArgumentExprs(): List[Tree] = {
      def args(): List[Tree] = commaSeparated {
        val maybeNamed = isIdent
        var isOutputParam   = false
        var isAdaptingParam = false
        var paramConstraint: Tree = null
        
        if (in.token == IDENTIFIER) in.name match {
          case nme.QMARKkw  => isOutputParam   = true; in.nextToken
          case nme.QMARK2kw => isAdaptingParam = true; in.nextToken
          case _ =>
        }
        
        var exp = expr() match {
          case a @ Assign(id, rhs) if maybeNamed => atPos(a.pos) { AssignOrNamedArg(id, rhs) }
          case e                                 => e
        }
        if (isOutputParam || isAdaptingParam) {
          if (in.token==IF_QMARK) {
             in.nextToken
             paramConstraint = simpleNativeValueExpr()
          }
        }
        if      (  isOutputParam) makeActualOutputParameter  (exp, paramConstraint)
        else if (isAdaptingParam) makeActualAdaptingParameter(exp, paramConstraint)
        else exp
      }
      inSubscriptArgumentParens(if (in.token == RPAREN) Nil else args())
    }
    
/* ----------- EXPRESSIONS ------------------------------------------------ */

    def condExpr(): Tree = {
      if (in.token == LPAREN) {
        in.nextToken()
        val r = expr()
        accept(RPAREN)
        r
      } else {
        accept(LPAREN)
        newLiteral(true)
      }
    }

    /* hook for IDE, unlike expression can be stubbed
     * don't use for any tree that can be inspected in the parser!
     */
    def statement(location: Location): Tree = expr(location) // !!! still needed?

    /** {{{
     *  Expr       ::= (Bindings | [`implicit'] Id | `_')  `=>' Expr
     *               | Expr1
     *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
     *               | Expr1
     *  Expr1      ::= if `(' Expr `)' {nl} Expr [[semi] else Expr]
     *               | try (`{' Block `}' | Expr) [catch `{' CaseClauses `}'] [finally Expr]
     *               | while `(' Expr `)' {nl} Expr
     *               | do Expr [semi] while `(' Expr `)'
     *               | for (`(' Enumerators `)' | `{' Enumerators `}') {nl} [yield] Expr
     *               | throw Expr
     *               | return [Expr]
     *               | [SimpleExpr `.'] Id `=' Expr
     *               | SimpleExpr1 ArgumentExprs `=' Expr
     *               | PostfixExpr Ascription
     *               | PostfixExpr match `{' CaseClauses `}'
     *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
     *  Binding    ::= (Id | `_') [`:' Type]
     *  Ascription ::= `:' CompoundType
     *               | `:' Annotation {Annotation}
     *               | `:' `_' `*'
     *  }}}
     */
    def expr(): Tree = expr(Local)

    def expr(location: Location): Tree = withPlaceholders(expr0(location), isAny = false)

    def expr0(location: Location): Tree = (in.token: @scala.annotation.switch) match {
      case IF =>
        def parseIf = atPos(in.skipToken()) {
          val cond  = condExpr(); newLinesOpt()
          val thenp = expr()
          val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                      else literalUnit
          If(cond, thenp, elsep)
        }
        parseIf
      case TRY =>
        def parseTry = atPos(in.skipToken()) {
          val body = in.token match {
            case LBRACE => inBracesOrUnit(block())
            case LPAREN => inParensOrUnit(expr ())
            case _ => expr()
          }
          def catchFromExpr() = List(makeCatchFromExpr(expr()))
          val catches: List[CaseDef] =
            if (in.token != CATCH) Nil
            else {
              in.nextToken()
              if (in.token != LBRACE) catchFromExpr()
              else inBracesOrNil {
                if (in.token == CASE) caseClauses()
                else catchFromExpr()
              }
            }
          val finalizer = in.token match {
            case FINALLY => in.nextToken(); expr()
            case _ => EmptyTree
          }
          Try(body, catches, finalizer)
        }
        parseTry
      case WHILE =>
        def parseWhile = {
          val start = in.offset
          atPos(in.skipToken()) {
            val cond = condExpr(); newLinesOpt()
            val body = expr()
            makeWhile(start, cond, body)
          }
        }
        parseWhile
      case DO =>
        def parseDo = {
          atPos(in.skipToken()) {
            val lname: Name = freshTermName(nme.DO_WHILE_PREFIX)
            val body        = expr()
            if (isStatSep) in.nextToken()
            accept(WHILE)
            val cond        = condExpr()
            makeDoWhile(lname.toTermName, body, cond)
          }
        }
        parseDo
      case FOR =>
        val start = in.skipToken()
        def parseFor = atPos(start) {
          val enums =
            if (in.token == LBRACE) inBracesOrNil(enumerators())
            else inParensOrNil(enumerators())
          newLinesOpt()
          if (in.token == YIELD) {
            in.nextToken()
            gen.mkFor(enums, gen.Yield(expr()))
          } else {
            gen.mkFor(enums, expr())
          }
        }
        def adjustStart(tree: Tree) =
          if (tree.pos.isRange && start < tree.pos.start)
            tree setPos tree.pos.withStart(start)
          else tree
        adjustStart(parseFor)
      case RETURN   => def parseReturn = atPos(in.skipToken()) {Return(if (isExprIntro) expr() else literalUnit)}
                       parseReturn
      case THROW    => def parseThrow  = atPos(in.skipToken()) {Throw(expr())}
                       parseThrow
      case IMPLICIT => implicitClosure(in.skipToken(), location)
      case _ =>
        def parseOther = {
          var t = postfixExpr()
          if (in.token == EQUALS) {
            t match {
              case Ident(_) | Select(_, _) | Apply(_, _) =>
                t = atPos(t.pos.start, in.skipToken()) { gen.mkAssign(t, expr()) }
              case _ =>
            }
          } else if (in.token == COLON) {
            t = stripParens(t)
            val colonPos = in.skipToken()
            if (in.token == USCORE) {
              //todo: need to handle case where USCORE is a wildcard in a type
              val uscorePos = in.skipToken()
              if (isIdent && in.name == nme.STAR) {
                in.nextToken()
                t = atPos(t.pos.start, colonPos) {
                  Typed(t, atPos(uscorePos) { Ident(tpnme.WILDCARD_STAR) })
                }
              } else {
                syntaxErrorOrIncomplete("`*' expected", skipIt = true)
              }
            } else if (isAnnotation) {
              t = (t /: annotations(skipNewLines = false))(makeAnnotated)
            } else {
              t = atPos(t.pos.start, colonPos) {
                val tpt = typeOrInfixType(location)
                if (isWildcard(t))
                  (placeholderParams: @unchecked) match {
                    case (vd @ ValDef(mods, name, _, _)) :: rest =>
                      placeholderParams = treeCopy.ValDef(vd, mods, name, tpt.duplicate, EmptyTree) :: rest
                  }
                // this does not correspond to syntax, but is necessary to
                // accept closures. We might restrict closures to be between {...} only.
                Typed(t, tpt)
              }
            }
          } else if (in.token == MATCH) {
            t = atPos(t.pos.start, in.skipToken())(Match(stripParens(t), inBracesOrNil(caseClauses())))
          }
          // in order to allow anonymous functions as statements (as opposed to expressions) inside
          // templates, we have to disambiguate them from self type declarations - bug #1565
          // The case still missed is unparenthesized single argument, like "x: Int => x + 1", which
          // may be impossible to distinguish from a self-type and so remains an error.  (See #1564)
          def lhsIsTypedParamList() = t match {
            case Parens(xs) if xs.forall(isTypedParam) => true
            case _ => false
          }
          if (in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)) {
            t = atPos(t.pos.start, in.skipToken()) {
              Function(convertToParams(t), if (location != InBlock) expr() else block())
            }
          }
          stripParens(t)
        }
        parseOther
    }

    def isTypedParam(t: Tree) = t.isInstanceOf[Typed]

    /** {{{
     *  Expr ::= implicit Id => Expr
     *  }}}
     */

    def implicitClosure(start: Offset, location: Location): Tree = {
      val param0 = convertToParam {
        atPos(in.offset) {
          Ident(ident()) match {
            case expr if in.token == COLON  => in.nextToken() ; Typed(expr, typeOrInfixType(location))
            case expr                       => expr
          }
        }
      }
      val param = copyValDef(param0)(mods = param0.mods | Flags.IMPLICIT)
      atPos(start, in.offset) {
        accept(ARROW)
        Function(List(param), if (location != InBlock) expr() else block())
      }
    }

    /** {{{
     *  PostfixExpr   ::= InfixExpr [Id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr Id [nl] InfixExpr
     *  }}}
     */
    def postfixExpr(): Tree = {
      val start = in.offset
      val base  = opstack

      def loop(top: Tree): Tree = if (!isIdent) top else {
        pushOpInfo(reduceExprStack(base, top))
        newLineOptWhenFollowing(isExprIntroToken)
        if (isExprIntro)
          prefixExpr() match {
            case EmptyTree => reduceExprStack(base, top)
            case next      => loop(next)
          }
        else finishPostfixOp(start, base, popOpInfo())
      }

      reduceExprStack(base, loop(prefixExpr()))
    }

    /** {{{
     *  PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
     *  }}}
     */
    def prefixExpr(): Tree = {
      if (isUnaryOp) {
        atPos(in.offset) {
          val name = nme.toUnaryName(rawIdent().toTermName)
          if (name == nme.UNARY_- && isNumericLit)
            simpleExprRest(literal(isNegated = true), canApply = true)
          else
            Select(stripParens(simpleExpr()), name)
        }
      }
      else simpleExpr()
    }
    def xmlLiteral(): Tree
    def scriptLiteral(doInBrackets: Boolean, simpleTermOnly: Boolean): Tree

    /** {{{
     *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
     *                  |  BlockExpr
     *                  |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                  |  xLiteral
     *                  |  Path
     *                  |  `(' [Exprs] `)'
     *                  |  SimpleExpr `.' Id
     *                  |  SimpleExpr TypeArgs
     *                  |  SimpleExpr1 ArgumentExprs
     *  }}}
     */
    def simpleExpr(): Tree = {
      var canApply = true
      val t =
        if (isLiteral) literal()
        else in.token match {
          case LBRACKET => canApply = false; scriptLiteral(doInBrackets=true, simpleTermOnly=false)
          case XMLSTART => xmlLiteral()
          case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER
                      => path(thisOK = true, typeOK = false)
          case USCORE =>
            freshPlaceholder()
          case LPAREN => atPos(in.offset)(makeParens(commaSeparated(expr())))
          case LBRACE => canApply = false; blockExpr()
          case NEW    => canApply = false
                         val nstart = in.skipToken()
                         val npos   = r2p(nstart, nstart, in.lastOffset)
                         val tstart = in.offset
                         val (parents, self, stats) = template()
                         val cpos   = r2p(tstart, tstart, in.lastOffset max tstart)
                         gen.mkNew(parents, self, stats, npos, cpos)
          case _      => syntaxErrorOrIncompleteAnd("illegal start of simple expression", skipIt = true)(errorTermTree)
        }
      simpleExprRest(t, canApply = canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT      => in.nextToken(); simpleExprRest(selector(stripParens(t)), canApply = true)
        case LBRACKET =>
          val t1 = stripParens(t)
          t1 match {
            case Ident(_) | Select(_, _) | Apply(_, _) =>
              var app: Tree = t1
              while (in.token == LBRACKET)
                app = atPos(app.pos.start, in.offset)(TypeApply(app, exprTypeArgs()))

              simpleExprRest(app, canApply = true)
            case _ => t1
          }
        case LPAREN | LBRACE if (canApply) =>
          val app = atPos(t.pos.start, in.offset) {
            // look for anonymous function application like (f _)(x) and
            // translate to (f _).apply(x), bug #460
            val sel = t match {
              case Parens(List(Typed(_, _: Function))) => Select(stripParens(t), nme.apply)
              case _                                   => stripParens(t)
            }
            Apply(sel, argumentExprs())
          }
          simpleExprRest(app, canApply = true)
        case USCORE => atPos(t.pos.start, in.skipToken()) {Typed(stripParens(t), Function(Nil, EmptyTree))}
        case _      => t
      }
    }

    
    /** {{{
     *  ArgumentExprs ::= `(' [Exprs] `)'
     *                  | [nl] BlockExpr
     *  }}}
     */
    def argumentExprs(): List[Tree] = {
      def args(): List[Tree] = commaSeparated(
        if (isIdent) treeInfo.assignmentToMaybeNamedArg(expr()) else expr()
      )
      in.token match {
        case LBRACE   => List(blockExpr())
        case LPAREN   => inParens(if (in.token == RPAREN) Nil else args())
        case _        => Nil
      }
    }
    /** A succession of argument lists. */
    def multipleArgumentExprs(): List[List[Tree]] = {
      if (in.token != LPAREN) Nil
      else argumentExprs() :: multipleArgumentExprs()
    }

    /** {{{
     *  BlockExpr ::= `{' (CaseClauses | Block) `}'
     *  }}}
     */
    def blockExpr(): Tree = atPos(in.offset) {
      inBraces {
        if (in.token == CASE) Match(EmptyTree, caseClauses())
        else block()
      }
    }

    /** {{{
     *  Block ::= BlockStatSeq
     *  }}}
     *  @note  Return tree does not carry position.
     */
    def block(): Tree = makeBlock(blockStatSeq())

    def caseClause(): CaseDef =
      atPos(in.offset)(makeCaseDef(pattern(), guard(), caseBlock()))

    /** {{{
     *  CaseClauses ::= CaseClause {CaseClause}
     *  CaseClause  ::= case Pattern [Guard] `=>' Block
     *  }}}
     */
    def caseClauses(): List[CaseDef] = {
      val cases = caseSeparated { caseClause() }
      if (cases.isEmpty)  // trigger error if there are no cases
        accept(CASE)

      cases
    }

    // IDE HOOK (so we can memoize case blocks) // needed?
    def caseBlock(): Tree =
      atPos(accept(ARROW))(block())

    /** {{{
     *  Guard ::= if PostfixExpr
     *  }}}
     */
    def guard(): Tree =
      if (in.token == IF) { in.nextToken(); stripParens(postfixExpr()) }
      else EmptyTree

    /** {{{
     *  Enumerators ::= Generator {semi Enumerator}
     *  Enumerator  ::=  Generator
     *                |  Guard
     *                |  val Pattern1 `=' Expr
     *  }}}
     */
    def enumerators(): List[Tree] = {
      val enums = new ListBuffer[Tree]
      enums ++= enumerator(isFirst = true)
      while (isStatSep) {
        in.nextToken()
        enums ++= enumerator(isFirst = false)
      }
      enums.toList
    }

    def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Tree] =
      if (in.token == IF && !isFirst) makeFilter(in.offset, guard()) :: Nil
      else generator(!isFirst, allowNestedIf)

    /** {{{
     *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
     *  }}}
     */
    def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Tree] = {
      val start  = in.offset
      val hasVal = in.token == VAL
      if (hasVal) in.nextToken()

      val pat    = noSeq.pattern1()
      val point  = in.offset
      val hasEq  = in.token == EQUALS

      if (hasVal) {if (hasEq) deprecationWarning(in.offset, "val keyword in for comprehension is deprecated")
                   else              syntaxError(in.offset, "val in for comprehension must be followed by assignment")}

      if (hasEq && eqOK) in.nextToken()
      else accept(LARROW)
      val rhs = expr()

      def loop(): List[Tree] =
        if (in.token != IF) Nil
        else makeFilter(in.offset, guard()) :: loop()

      val tail = if (allowNestedIf) loop()
                 else Nil

      // why max? IDE stress tests have shown that lastOffset could be less than start,
      // I guess this happens if instead if a for-expression we sit on a closing paren.
      val genPos = r2p(start, point, in.lastOffset max start)
      gen.mkGenerator(genPos, pat, hasEq, rhs) :: tail
    }

    def makeFilter(start: Offset, tree: Tree) = gen.Filter(tree).setPos(r2p(start, tree.pos.point, tree.pos.end))

/* -------- PATTERNS ------------------------------------------- */

    /** Methods which implicitly propagate whether the initial call took
     *  place in a context where sequences are allowed.  Formerly, this
     *  was threaded through methods as boolean seqOK.
     */
    trait SeqContextSensitive extends PatternContextSensitive {
      // is a sequence pattern _* allowed?
      def isSequenceOK: Boolean

      // are we in an XML pattern?
      def isXML: Boolean = false

      def functionArgType(): Tree = argType()
      def argType(): Tree = {
        val start = in.offset
        in.token match {
          case USCORE =>
            in.nextToken()
            if (in.token == SUBTYPE || in.token == SUPERTYPE) wildcardType(start)
            else atPos(start) { Bind(tpnme.WILDCARD, EmptyTree) }
          case _ =>
            typ() match {
              case Ident(name: TypeName) if nme.isVariableName(name) =>
                atPos(start) { Bind(name, EmptyTree) }
              case t => t
            }
        }
      }

      /** {{{
       *  Patterns ::= Pattern { `,' Pattern }
       *  SeqPatterns ::= SeqPattern { `,' SeqPattern }
       *  }}}
       */
      def patterns(): List[Tree] = commaSeparated(pattern())

      /** {{{
       *  Pattern  ::=  Pattern1 { `|' Pattern1 }
       *  SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
       *  }}}
       */
      def pattern(): Tree = {
        val start = in.offset
        def loop(): List[Tree] = pattern1() :: {
          if (isRawBar) { in.nextToken() ; loop() }
          else Nil
        }
        loop() match {
          case pat :: Nil => pat
          case xs         => atPos(start)(makeAlternative(xs))
        }
      }

      /** {{{
       *  Pattern1    ::= varid `:' TypePat
       *                |  `_' `:' TypePat
       *                |  Pattern2
       *  SeqPattern1 ::= varid `:' TypePat
       *                |  `_' `:' TypePat
       *                |  [SeqPattern2]
       *  }}}
       */
      def pattern1(): Tree = pattern2() match {
        case p @ Ident(name) if in.token == COLON =>
          if (treeInfo.isVarPattern(p))
            atPos(p.pos.start, in.skipToken())(Typed(p, compoundType()))
          else {
            syntaxError(in.offset, "Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
            p
          }
        case p => p
      }

      /** {{{
       *  Pattern2    ::=  varid [ @ Pattern3 ]
       *                |   Pattern3
       *  SeqPattern2 ::=  varid [ @ SeqPattern3 ]
       *                |   SeqPattern3
       *  }}}
       */
      def pattern2(): Tree = {
        val p = pattern3()

        if (in.token != AT) p
        else p match {
          case Ident(nme.WILDCARD) =>
            in.nextToken()
            pattern3()
          case Ident(name) if treeInfo.isVarPattern(p) =>
            in.nextToken()
            atPos(p.pos.start) { Bind(name, pattern3()) }
          case _ => p
        }
      }

      /** {{{
       *  Pattern3    ::= SimplePattern
       *                |  SimplePattern {Id [nl] SimplePattern}
       *  }}}
       */
      def pattern3(): Tree = {
        val top = simplePattern(badPattern3)
        val base = opstack
        // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
        def isCloseDelim = in.token match {
          case RBRACE => isXML
          case RPAREN => !isXML
          case _      => false
        }
        def checkWildStar: Tree = top match {
          case Ident(nme.WILDCARD) if isSequenceOK && isRawStar => peekingAhead (
            if (isCloseDelim) atPos(top.pos.start, in.prev.offset)(Star(stripParens(top)))
            else EmptyTree
          )
          case _ => EmptyTree
        }
        def loop(top: Tree): Tree = reducePatternStack(base, top) match {
          case next if isIdentExcept(raw.BAR) => pushOpInfo(next) ; loop(simplePattern(badPattern3))
          case next                           => next
        }
        checkWildStar orElse stripParens(loop(top))
      }

      def badPattern3(): Tree = {
        def isComma                = in.token == COMMA
        def isDelimiter            = in.token == RPAREN || in.token == RBRACE
        def isCommaOrDelimiter     = isComma || isDelimiter
        val (isUnderscore, isStar) = opstack match {
          case OpInfo(Ident(nme.WILDCARD), nme.STAR, _, _) :: _ => (true,   true)
          case OpInfo(_, nme.STAR, _, _) :: _                   => (false,  true)
          case _                                                => (false, false)
        }
        def isSeqPatternClose = isUnderscore && isStar && isSequenceOK && isDelimiter
        val preamble = "bad simple pattern:"
        val subtext = (isUnderscore, isStar, isSequenceOK) match {
          case (true,  true, true)  if isComma            => "bad use of _* (a sequence pattern must be the last pattern)"
          case (true,  true, true)  if isDelimiter        => "bad brace or paren after _*"
          case (true,  true, false) if isDelimiter        => "bad use of _* (sequence pattern not allowed)"
          case (false, true, true)  if isDelimiter        => "use _* to match a sequence"
          case (false, true, _)     if isCommaOrDelimiter => "trailing * is not a valid pattern"
          case _                                          => null
        }
        val msg = if (subtext != null) s"$preamble $subtext" else "illegal start of simple pattern"
        // better recovery if don't skip delims of patterns
        val skip = !isCommaOrDelimiter || isSeqPatternClose
        syntaxErrorOrIncompleteAnd(msg, skip)(errorPatternTree)
      }

      /** {{{
       *  SimplePattern    ::= varid
       *                    |  `_'
       *                    |  literal
       *                    |  XmlPattern
       *                    |  StableId  /[TypeArgs]/ [`(' [Patterns] `)']
       *                    |  StableId  [`(' [Patterns] `)']
       *                    |  StableId  [`(' [Patterns] `,' [varid `@'] `_' `*' `)']
       *                    |  `(' [Patterns] `)'
       *  }}}
       *
       * XXX: Hook for IDE
       */
      def simplePattern(): Tree = (
        // simple diagnostics for this entry point
        simplePattern(() => syntaxErrorOrIncompleteAnd("illegal start of simple pattern", skipIt = true)(errorPatternTree))
      )
      def simplePattern(onError: () => Tree): Tree = {
        val start = in.offset
        in.token match {
          case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
            val t = stableId()
            in.token match {
              case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
                t match {
                  case Ident(nme.MINUS) => return literal(isNegated = true, inPattern = true, start = start)
                  case _ =>
                }
              case _ =>
            }
            val typeAppliedTree = in.token match {
              case LBRACKET => atPos(start, in.offset)(AppliedTypeTree(convertToTypeId(t), typeArgs()))
              case _        => t
            }
            in.token match {
              case LPAREN   => atPos(start, in.offset)(Apply(typeAppliedTree, argumentPatterns()))
              case _        => typeAppliedTree
            }
          case USCORE => in.nextToken(); atPos(start, start) { Ident(nme.WILDCARD) }
          case CHARLIT 
          |     INTLIT 
          |    LONGLIT 
          |   FLOATLIT 
          |  DOUBLELIT 
          |  STRINGLIT 
          | INTERPOLATIONID 
          |  SYMBOLLIT 
          |       TRUE 
          |      FALSE 
          |       NULL  => literal(inPattern = true)
          case LPAREN   => atPos(start)(makeParens(noSeq.patterns()))
          case XMLSTART => xmlLiteralPattern()
          case _        => onError()
        }
      }
    }
    /** The implementation of the context sensitive methods for parsing outside of patterns. */
    object outPattern extends PatternContextSensitive {
      def argType(): Tree = typ()
      def functionArgType(): Tree = paramType(useStartAsPosition = true)
    }
    /** The implementation for parsing inside of patterns at points where sequences are allowed. */
    object seqOK extends SeqContextSensitive {
      val isSequenceOK = true
    }
    /** The implementation for parsing inside of patterns at points where sequences are disallowed. */
    object noSeq extends SeqContextSensitive {
      val isSequenceOK = false
    }
    /** For use from xml pattern, where sequence is allowed and encouraged. */
    object xmlSeqOK extends SeqContextSensitive {
      val isSequenceOK = true
      override val isXML = true
    }
    /** These are default entry points into the pattern context sensitive methods:
     *  they are all initiated from non-pattern context.
     */
    def typ(): Tree      = outPattern.typ()
    def startInfixType() = outPattern.infixType(InfixMode.FirstOp)
    def startAnnotType() = outPattern.annotType()
    def exprTypeArgs  () = outPattern.typeArgs()
    def exprSimpleType() = outPattern.simpleType()

    /** Default entry points into some pattern contexts. */
    def           pattern ():      Tree  =  noSeq  .pattern()
    def        seqPatterns(): List[Tree] =    seqOK.patterns()
    def     xmlSeqPatterns(): List[Tree] = xmlSeqOK.patterns() // Called from xml parser
    def   argumentPatterns(): List[Tree] = inParens {if (in.token == RPAREN) Nil else seqPatterns()}
    def xmlLiteralPattern (): Tree

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    /** Drop `private` modifier when followed by a qualifier.
     *  Contract `abstract` and `override` to ABSOVERRIDE
     */
    private def normalizeModifers(mods: Modifiers): Modifiers =
      if (mods.isPrivate && mods.hasAccessBoundary)                normalizeModifers(mods &~  Flags.PRIVATE)
      else if (mods hasAllFlags (Flags.ABSTRACT | Flags.OVERRIDE)) normalizeModifers(mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE)
      else mods

    private def addMod(mods: Modifiers, mod: Long, pos: Position): Modifiers = {
      if (mods hasFlag mod) syntaxError(in.offset, "repeated modifier", skipIt = false)
      in.nextToken()
      (mods | mod) withPosition (mod, pos)
    }

    private def tokenRange(token: TokenData) = r2p(token.offset, token.offset, token.offset + token.name.length - 1)

    /** {{{
     *  AccessQualifier ::= `[' (Id | this) `]'
     *  }}}
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers = {
      var result = mods
      if (in.token == LBRACKET) {
        in.nextToken()
        if (mods.hasAccessBoundary) syntaxError("duplicate private/protected qualifier", skipIt = false)
        result = if (in.token == THIS) { in.nextToken(); mods | Flags.LOCAL }
                 else Modifiers(mods.flags, identForType())
        accept(RBRACKET)
      }
      result
    }

    private val flagTokens: Map[Int, Long] = Map(
      ABSTRACT  -> Flags.ABSTRACT,
      FINAL     -> Flags.FINAL,
      IMPLICIT  -> Flags.IMPLICIT,
      LAZY      -> Flags.LAZY,
      OVERRIDE  -> Flags.OVERRIDE,
      PRIVATE   -> Flags.PRIVATE,
      PROTECTED -> Flags.PROTECTED,
      SEALED    -> Flags.SEALED
    )

    /** {{{
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  }}}
     */
    def accessModifierOpt(): Modifiers = normalizeModifers {
      in.token match {
        case m @ (PRIVATE | PROTECTED)  => in.nextToken() ; accessQualifierOpt(Modifiers(flagTokens(m)))
        case _                          => NoMods
      }
    }

    /** {{{
     *  Modifiers ::= {Modifier}
     *  Modifier  ::= LocalModifier
     *              |  AccessModifier
     *              |  override
     *  }}}
     */
    def modifiers(): Modifiers = normalizeModifers {
      def loop(mods: Modifiers): Modifiers = in.token match {
        case PRIVATE 
           | PROTECTED => loop(accessQualifierOpt(addMod(mods, flagTokens(in.token), tokenRange(in))))
        case ABSTRACT 
           | FINAL 
           | SEALED 
           | OVERRIDE 
           | IMPLICIT 
           | LAZY      => loop(addMod(mods, flagTokens(in.token), tokenRange(in)))
        case NEWLINE   => in.nextToken(); loop(mods)
        case _         => mods
      }
      loop(NoMods)
    }

    /** {{{
     *  LocalModifiers ::= {LocalModifier}
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     *  }}}
     */
    def localModifiers(): Modifiers = {
      def loop(mods: Modifiers): Modifiers =
        if (isLocalModifier) loop(addMod(mods, flagTokens(in.token), tokenRange(in)))
        else mods

      loop(NoMods)
    }

    /** {{{
     *  Annotations      ::= {`@' SimpleType {ArgumentExprs}}
     *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
     *  }}}
     */
    def annotations(skipNewLines: Boolean): List[Tree] = readAnnots {
      val t = annotationExpr()
      if (skipNewLines) newLineOpt()
      t
    }
    def constructorAnnotations(): List[Tree] = readAnnots {
      atPos(in.offset)(New(exprSimpleType(), List(argumentExprs())))
    }

    def annotationExpr(): Tree = atPos(in.offset) {
      val t = exprSimpleType()
      if (in.token == LPAREN) New(t, multipleArgumentExprs())
      else New(t, Nil)
    }

/* -------- PARAMETERS ------------------------------------------- */


    /** {{{
     *  ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
     *  ParamClause       ::= [nl] `(' [Params] `)'
     *  Params            ::= Param {`,' Param}
     *  Param             ::= {Annotation} Id [`:' ParamType] [`=' Expr]
     *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
     *  ClassParamClause  ::= [nl] `(' [ClassParams] `)'
     *  ClassParams       ::= ClassParam {`,' ClassParam}
     *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType] [`=' Expr]
     *  }}}
     */
    def paramClauses(owner: Name, contextBounds: List[Tree], ofCaseClass: Boolean): List[List[ValDef]] = {
      var implicitmod = 0
      var caseParam = ofCaseClass
      def paramClause(): List[ValDef] = {
        if (in.token == RPAREN  ) return Nil
        if (in.token == IMPLICIT) {in.nextToken(); implicitmod = Flags.IMPLICIT}
        commaSeparated(param(owner, implicitmod, caseParam  ))
      }
      val vds   = new ListBuffer[List[ValDef]]
      val start = in.offset
      newLineOptWhenFollowedBy(LPAREN)
      if (ofCaseClass && in.token != LPAREN)
        syntaxError(in.lastOffset, "case classes without a parameter list are not allowed;\n"+
                                   "use either case objects or case classes with an explicit `()' as a parameter list.")
      while (implicitmod == 0 && in.token == LPAREN) {
        in.nextToken()
        vds += paramClause()
        accept(RPAREN)
        caseParam = false
        newLineOptWhenFollowedBy(LPAREN)
      }
      val result = vds.toList
      if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
        in.token match {
          case LBRACKET   => syntaxError(in.offset, "no type parameters allowed here", skipIt = false)
          case EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
          case _          => syntaxError(start, "auxiliary constructor needs non-implicit parameter list", skipIt = false)
        }
      }
      addEvidenceParams(owner, result, contextBounds)
    }

    /** {{{
     *  ParamType ::= Type | `=>' Type | Type `*'
     *  }}}
     */
    def paramType(): Tree = paramType(useStartAsPosition = false)
    def paramType(useStartAsPosition: Boolean): Tree = {
      val start = in.offset
      in.token match {
        case ARROW  =>
          in.nextToken()
          atPos(start)(byNameApplication(typ()))
        case _      =>
          val t = typ()
          if (isRawStar) {
            in.nextToken()
            if (useStartAsPosition) atPos(start)(repeatedApplication(t))
            else atPos(t.pos.start, t.pos.point)(repeatedApplication(t))
          }
          else t
      }
    }

    def param(owner: Name, implicitmod: Int, caseParam: Boolean): ValDef = {
      val start  = in.offset
      var annots = annotations(skipNewLines = false)
      var mods   = Modifiers(Flags.PARAM)
      if (owner.isTypeName) {
        mods = modifiers() | Flags.PARAMACCESSOR
        if (mods.isLazy) syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead", skipIt = false)
        in.token match {
          case v @ (VAL | VAR) =>
            mods = mods withPosition (in.token.toLong, tokenRange(in))
            if (v == VAR) mods |= Flags.MUTABLE
            in.nextToken()
          case _ =>
            if (mods.flags != Flags.PARAMACCESSOR) accept(VAL)
            if (!caseParam) mods |= Flags.PrivateLocal
        }
        if (caseParam) mods |= Flags.CASEACCESSOR
      }
      var isSubScriptFormalOutputParameter      = false
      var isSubScriptFormalConstrainedParameter = false
      if (in.isInSubScript_header) { // TBD: clean up
        if (in.token==IDENTIFIER)
          in.name match {
            case nme.QMARKkw  => isSubScriptFormalOutputParameter      = true; in.nextToken()
            case nme.QMARK2kw => isSubScriptFormalConstrainedParameter = true; in.nextToken()
            case _            =>
          }
      }
      val nameOffset = in.offset
      val name       = ident()
      var bynamemod = 0
      val tpt =
        if ((settings.YmethodInfer && !owner.isTypeName) && in.token != COLON) {
          TypeTree()
        } else { // XX-METHOD-INFER
          accept(COLON)
          if (in.token == ARROW) {
            if (owner.isTypeName && !mods.isLocalToThis)
                                       syntaxError(in.offset, (if (mods.isMutable) "`var'" else "`val'") + " parameters may not be call-by-name", skipIt = false)
            else if (implicitmod != 0) syntaxError(in.offset,                                      "implicit parameters may not be call-by-name", skipIt = false)
            else                       bynamemod = Flags.BYNAMEPARAM
          }
          paramType()
        }
      val default =
        if (in.token == EQUALS) {
          in.nextToken()
          mods |= Flags.DEFAULTPARAM
          expr()
        } else EmptyTree
      val termName = name.toTermName
      
      if (isSubScriptFormalOutputParameter     ) storeScriptFormalOutputParameter     (termName,tpt)
      if (isSubScriptFormalConstrainedParameter) storeScriptFormalConstrainedParameter(termName,tpt)
      
      atPos(start, if (name == nme.ERROR) start else nameOffset) {
        ValDef((mods | implicitmod.toLong | bynamemod) withAnnotations annots, name.toTermName, tpt, default)
      }
    }

    /** {{{
     *  TypeParamClauseOpt    ::= [TypeParamClause]
     *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
     *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
     *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
     *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
     *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {<% Type} {":" Type}
     *  }}}
     */
    def typeParamClauseOpt(owner: Name, contextBoundBuf: ListBuffer[Tree]): List[TypeDef] = {
      def typeParam(ms: Modifiers): TypeDef = {
        var mods = ms | Flags.PARAM
        val start = in.offset
        if (owner.isTypeName && isIdent) {
          if      (in.name == raw.PLUS ) {in.nextToken(); mods |= Flags.    COVARIANT}
          else if (in.name == raw.MINUS) {in.nextToken(); mods |= Flags.CONTRAVARIANT}
        }
        val nameOffset = in.offset
        // TODO AM: freshTermName(o2p(in.skipToken()), "_$$"), will need to update test suite
        val pname: TypeName = wildcardOrIdent().toTypeName
        val param = atPos(start, nameOffset) {
          val tparams = typeParamClauseOpt(pname, null) // @M TODO null --> no higher-order context bounds for now
          TypeDef(mods, pname, tparams, typeBounds())
        }
        if (contextBoundBuf ne null) {
          while (in.token == VIEWBOUND) {
            val msg = "Use an implicit parameter instead.\nExample: Instead of `def f[A <% Int](a: A)` use `def f[A](a: A)(implicit ev: A => Int)`."
            if (settings.future)
              deprecationWarning(in.offset, s"View bounds are deprecated. $msg")
            contextBoundBuf += atPos(in.skipToken())(makeFunctionTypeTree(List(Ident(pname)), typ()))
          }
          while (in.token == COLON) {
            contextBoundBuf += atPos(in.skipToken()) {
              AppliedTypeTree(typ(), List(Ident(pname)))
            }
          }
        }
        param
      }
      newLineOptWhenFollowedBy(LBRACKET)
      if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(NoMods withAnnotations annotations(skipNewLines = true))))
      else Nil
    }

    /** {{{
     *  TypeBounds ::= [`>:' Type] [`<:' Type]
     *  }}}
     */
    def typeBounds(): TypeBoundsTree = {
      val lo      = bound(SUPERTYPE)
      val hi      = bound(SUBTYPE)
      val t       = TypeBoundsTree(lo, hi)
      val defined = List(t.hi, t.lo) filter (_.pos.isDefined)

      if (defined.nonEmpty) t setPos wrappingPos(defined)
      else                  t setPos o2p(in.offset)
    }

    def bound(tok: Token): Tree = if (in.token == tok) { in.nextToken(); typ() } else EmptyTree

/* -------- DEFS ------------------------------------------- */


    /** {{{
     *  Import  ::= import ImportExpr {`,' ImportExpr}
     *  }}}
     */
    def importClause(): List[Tree] = {
      val offset = accept(IMPORT)
      commaSeparated(importExpr()) match {
        case Nil => Nil
        case t :: rest =>
          // The first import should start at the position of the keyword.
          t.setPos(t.pos.withStart(offset))
          t :: rest
      }
    }

    /** {{{
     *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     *  }}}
     */
    def importExpr(): Tree = {
      val start = in.offset
      def thisDotted(name: TypeName) = {
        in.nextToken(); val t = atPos(start)(This(name))
        accept(DOT)   ; val result = selector(t)
        accept(DOT)   ;     result
      }
      /* Walks down import `foo.bar.baz.{ ... }` until it ends at a
       * an underscore, a left brace, or an undotted identifier.
       */
      def loop(expr: Tree): Tree = {
        expr setPos expr.pos.makeTransparent
        val selectors: List[ImportSelector] = in.token match {
          case USCORE   => List(importSelector()) // import foo.bar._;
          case LBRACE   => importSelectors()      // import foo.bar.{ x, y, z }
          case _        =>
            val nameOffset = in.offset
            val name = ident()
            if (in.token == DOT) {
              // import foo.bar.ident.<unknown> and so create a select node and recurse.
              val t = atPos(start, if (name == nme.ERROR) in.offset else nameOffset)(Select(expr, name))
              in.nextToken()
              return loop(t)
            }
            // import foo.bar.Baz;
            else List(makeImportSelector(name, nameOffset))
        }
        // reaching here means we're done walking.
        atPos(start)(Import(expr, selectors))
      }

      loop(in.token match {
        case THIS   => thisDotted(tpnme.EMPTY)
        case _      => val id = atPos(start)(Ident(ident()))
                       accept(DOT)
                       if (in.token == THIS) thisDotted(id.name.toTypeName)
                       else id
      })
    }

    /** {{{
     *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     *  }}}
     */
    def importSelectors(): List[ImportSelector] = {
      val selectors = inBracesOrNil(commaSeparated(importSelector()))
      selectors.init foreach {
        case ImportSelector(nme.WILDCARD, pos, _, _)  => syntaxError(pos, "Wildcard import must be in last position")
        case _                                        => ()
      }
      selectors
    }

    def wildcardOrIdent() = {
      if (in.token == USCORE) { in.nextToken() ; nme.WILDCARD }
      else ident()
    }

    /** {{{
     *  ImportSelector ::= Id [`=>' Id | `=>' `_']
     *  }}}
     */
    def importSelector(): ImportSelector = {
      val start        = in.offset
      val name         = wildcardOrIdent()
      var renameOffset = -1
      val rename       = in.token match {
         case ARROW                     => in.nextToken(); renameOffset = in.offset; wildcardOrIdent()
         case _ if name == nme.WILDCARD => null
         case _                         => renameOffset = start; name
      }
      ImportSelector(name, start, rename, renameOffset)
    }

    /** {{{
     *  Def    ::= val PatDef
     *           | var PatDef
     *           | def FunDef
     *           | type [nl] TypeDef
     *           | TmplDef
     *  Dcl    ::= val PatDcl
     *           | var PatDcl
     *           | def FunDcl
     *           | type [nl] TypeDcl
     *  }}}
     */
    def defOrDcl(pos: Offset, mods: Modifiers): List[Tree] = {
      if (mods.isLazy && in.token != VAL)
        syntaxError("lazy not allowed here. Only vals can be lazy", skipIt = false)
      in.token match {
        case VAL    =>       patDefOrDcl (pos,  mods                  withPosition(VAL , tokenRange(in)))
        case VAR    =>       patDefOrDcl (pos, (mods | Flags.MUTABLE) withPosition(VAR , tokenRange(in)))
        case DEF    => 
                  in.nextTokenAllow(nme.SCRIPTkw) // TBD: use script_Name instead?
                  if (isScriptIdent) {
                         in.linePosOfScriptsSection = pos - in.lineStartOffset
                         scriptDefsOrDcls(pos,  mods                  withPosition(DEF , tokenRange(in)))
                  }
                  else List( funDefOrDcl (pos,  mods                  withPosition(DEF , tokenRange(in))))
        case TYPE   => List(typeDefOrDcl (pos,  mods                  withPosition(TYPE, tokenRange(in))))
        case _      => List(tmplDef      (pos, mods))
      }
    }

    private def caseAwareTokenOffset = if (in.token == CASECLASS || in.token == CASEOBJECT) in.prev.offset else in.offset

    def nonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(skipNewLines = true)
      defOrDcl(caseAwareTokenOffset, modifiers() withAnnotations annots)
    }

    /** {{{
     *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  ValDcl ::= Id {`,' Id} `:' Type
     *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
     *  }}}
     */
    def patDefOrDcl(pos : Int, mods: Modifiers): List[Tree] = {
      var newmods = mods
      in.nextToken()
      val lhs = commaSeparated(stripParens(noSeq.pattern2()))
      val tp = typedOpt()
      val rhs =
        if (tp.isEmpty || in.token == EQUALS) {
          accept(EQUALS)
          if (!tp.isEmpty && newmods.isMutable &&
              (lhs.toList forall (_.isInstanceOf[Ident])) && in.token == USCORE) {
            in.nextToken()
            newmods = newmods | Flags.DEFAULTINIT; EmptyTree}
          else {expr()}
        }
        else {newmods = newmods | Flags.DEFERRED; EmptyTree}
      def mkDefs(p: Tree, tp: Tree, rhs: Tree): List[Tree] = {
        val trees = {
          val pat = if (tp.isEmpty) p else Typed(p, tp) setPos (p.pos union tp.pos)
          gen.mkPatDef(newmods, pat, rhs)
        }
        if (newmods.isDeferred) {
          trees match {
            case List(ValDef(_, _, _, EmptyTree)) =>
              if (mods.isLazy) syntaxError(p.pos, "lazy values may not be abstract", skipIt = false)
            case _ => syntaxError(p.pos, "pattern definition may not be abstract", skipIt = false)
          }
        }
        trees
      }
      val trees = (lhs.toList.init flatMap (mkDefs(_, tp.duplicate, rhs.duplicate))) ::: mkDefs(lhs.last, tp, rhs)
      val hd    = trees.head
      hd setPos hd.pos.withStart(pos)
      ensureNonOverlapping(hd, trees.tail)
      trees
    }

    /** {{{
     *  VarDef ::= PatDef
     *           | Id {`,' Id} `:' Type `=' `_'
     *  VarDcl ::= Id {`,' Id} `:' Type
     *  }}}
    def varDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods | Flags.MUTABLE
      val lhs = new ListBuffer[(Int, Name)]
      do {
        in.nextToken()
        lhs += (in.offset, ident())
      } while (in.token == COMMA)
      val tp = typedOpt()
      val rhs = if (tp.isEmpty || in.token == EQUALS) {
        accept(EQUALS)
        if (!tp.isEmpty && in.token == USCORE) {
          in.nextToken()
          EmptyTree
        } else {
          expr()
        }
      } else {
        newmods = newmods | Flags.DEFERRED
        EmptyTree
      }
    }
     */

    /** {{{
     *  FunDef ::= FunSig [`:' Type] `=' [`macro'] Expr
     *          |  FunSig [nl] `{' Block `}'
     *          |  `this' ParamClause ParamClauses
     *                 (`=' ConstrExpr | [nl] ConstrBlock)
     *  FunDcl ::= FunSig [`:' Type]
     *  FunSig ::= id [FunTypeParamClause] ParamClauses
     *  }}}
     */
    def funDefOrDcl(start : Int, mods: Modifiers): Tree = {
      // in.nextToken - this is now done at caller (defOrDcl)
      if (in.token == THIS) {
        atPos(start, in.skipToken()) {
          val vparamss = paramClauses(nme.CONSTRUCTOR, classContextBounds map (_.duplicate), ofCaseClass = false)
          newLineOptWhenFollowedBy(LBRACE)
          val rhs = in.token match {
            case LBRACE => atPos(in.offset) { constrBlock(vparamss) }
            case _      => accept(EQUALS) ; atPos(in.offset) { constrExpr(vparamss) }
          }
          DefDef(mods, nme.CONSTRUCTOR, List(), vparamss, TypeTree(), rhs)
        }
      }
      else {
        val nameOffset = in.offset
        val name       = identOrMacro()
        funDefRest(start, nameOffset, mods, name)
      }
    }

    def funDefRest(start: Offset, nameOffset: Offset, mods: Modifiers, name: Name): Tree = {
      val result = atPos(start, if (name.toTermName == nme.ERROR) start else nameOffset) {
        var newmods = mods
        // contextBoundBuf is for context bounded type parameters of the form
        // [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
        // i.e. (B[T] or T => B)
        val contextBoundBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, contextBoundBuf)
        val vparamss = paramClauses(name, contextBoundBuf.toList, ofCaseClass = false)
        newLineOptWhenFollowedBy(LBRACE)
        var restype = fromWithinReturnType(typedOpt())
        val rhs =
          if (isStatSep || in.token == RBRACE) {
            if (restype.isEmpty) {
              if (settings.future)
                deprecationWarning(in.lastOffset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
              restype = scalaUnitConstr
            }
            newmods |= Flags.DEFERRED
            EmptyTree
          } else if (restype.isEmpty && in.token == LBRACE) {
            if (settings.future)
              deprecationWarning(in.offset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit =`.")
            restype = scalaUnitConstr
            blockExpr()
          } else {
            if (in.token == EQUALS) {
              in.nextTokenAllow(nme.MACROkw)
              if (isMacro) {
                in.nextToken()
                newmods |= Flags.MACRO
              }
            } else {
              accept(EQUALS)
            }
            expr()
          }
        DefDef(newmods, name.toTermName, tparams, vparamss, restype, rhs)
      }
      signalParseProgress(result.pos)
      result
    }

    /** {{{
     *  ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     *  }}}
     */
    def constrExpr(vparamss: List[List[ValDef]]): Tree =
      if (in.token == LBRACE) constrBlock(vparamss)
      else Block(selfInvocation(vparamss) :: Nil, literalUnit)

    /** {{{
     *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     *  }}}
     */
    def selfInvocation(vparamss: List[List[ValDef]]): Tree =
      atPos(accept(THIS)) {                                     newLineOptWhenFollowedBy(LBRACE)
        var t = Apply(Ident(nme.CONSTRUCTOR), argumentExprs()); newLineOptWhenFollowedBy(LBRACE)
        while (in.token == LPAREN 
           ||  in.token == LBRACE) {
            t = Apply(t,                      argumentExprs()); newLineOptWhenFollowedBy(LBRACE)
        }
        if (classContextBounds.isEmpty) t
        else    Apply(t, vparamss.last.map(vp => Ident(vp.name)))
      }

    /** {{{
     *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     *  }}}
     */
    def constrBlock(vparamss: List[List[ValDef]]): Tree =
      atPos(in.skipToken()) {
        val stats = selfInvocation(vparamss) :: {
          if (isStatSep) { in.nextToken(); blockStatSeq() }
          else Nil
        }
        accept(RBRACE)
        Block(stats, literalUnit)
      }

    /** {{{
     *  TypeDef ::= type Id [TypeParamClause] `=' Type
     *            | FunSig `=' Expr
     *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
     *  }}}
     */
    def typeDefOrDcl(start: Offset, mods: Modifiers): Tree = {
      in.nextToken()
      newLinesOpt()
      atPos(start, in.offset) {
        val name = identForType()
        // @M! a type alias as well as an abstract type may declare type parameters
        val tparams = typeParamClauseOpt(name, null)
        in.token match {
          case EQUALS => in.nextToken(); TypeDef(mods, name, tparams, typ())
          case t if t == SUPERTYPE 
                 || t == SUBTYPE 
// These 3 had been inserted for SubScript; deactivated since the parser failed to compile regular Scala code. TBD: Check validity
//             || t == SEMI 
//             || t == NEWLINE 
//             || t == NEWLINES 
//////////////////
                 || t == COMMA 
                 || t == RBRACE 
                 || isStatSep(t) => TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds())
          case _  => syntaxErrorOrIncompleteAnd("`=', `>:', or `<:' expected", skipIt = true)(EmptyTree)
        }
      }
    }

    /** Hook for IDE, for top-level classes/objects. */
    def topLevelTmplDef: Tree = {
      val annots = annotations(skipNewLines = true)
      val pos    = caseAwareTokenOffset
      val mods   = modifiers() withAnnotations annots
      tmplDef(pos, mods)
    }

    /** {{{
     *  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  [override] trait TraitDef
     *  }}}
     */
    def tmplDef(pos: Offset, mods: Modifiers): Tree = {
      if (mods.isLazy) syntaxError("classes cannot be lazy", skipIt = false)
      in.token match {
        case     TRAIT  =>  classDef(pos, (mods | Flags.TRAIT | Flags.ABSTRACT) withPosition (Flags.TRAIT, tokenRange(in)))
        case     CLASS  =>  classDef(pos,  mods)
        case CASECLASS  =>  classDef(pos, (mods | Flags.CASE) withPosition (Flags.CASE, tokenRange(in.prev /*scanner skips on 'case' to 'class', thus take prev*/)))
        case     OBJECT => objectDef(pos,  mods)
        case CASEOBJECT => objectDef(pos, (mods | Flags.CASE) withPosition (Flags.CASE, tokenRange(in.prev /*scanner skips on 'case' to 'object', thus take prev*/)))
        case _          => syntaxErrorOrIncompleteAnd("expected start of definition", skipIt = true)(EmptyTree)
      }
    }

    /** {{{
     *  ClassDef ::= Id [TypeParamClause] {Annotation}
     *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
     *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
     *  }}}
     */
    def classDef(start: Offset, mods: Modifiers): ClassDef = {
      in.nextToken()
      val nameOffset = in.offset
      val name       = identForType()
      atPos(start, if (name == tpnme.ERROR) start else nameOffset) {
        savingClassContextBounds {
          val contextBoundBuf = new ListBuffer[Tree]
          val tparams        = typeParamClauseOpt(name, contextBoundBuf)
          classContextBounds = contextBoundBuf.toList
          val tstart = (in.offset :: classContextBounds.map(_.pos.start)).min
          if (!classContextBounds.isEmpty && mods.isTrait) {
            val viewBoundsExist = if (settings.future) "" else " nor view bounds `<% ...'"
              syntaxError(s"traits cannot have type parameters with context bounds `: ...'$viewBoundsExist", skipIt = false)
            classContextBounds = List()
          }
          val constrAnnots = if (!mods.isTrait) constructorAnnotations() else Nil
          val (constrMods, vparamss) =
            if (mods.isTrait) (Modifiers(Flags.TRAIT), List())
            else (accessModifierOpt(), paramClauses(name, classContextBounds, ofCaseClass = mods.isCase))
          var mods1 = mods
          if (mods.isTrait) {
            if (settings.YvirtClasses && in.token == SUBTYPE) mods1 |= Flags.DEFERRED
          } else if (in.token == SUBTYPE) {
            syntaxError("classes are not allowed to be virtual", skipIt = false)
          }
          val template = templateOpt(mods1, name, constrMods withAnnotations constrAnnots, vparamss, tstart)
          val result = gen.mkClassDef(mods1, name, tparams, template)
          // Context bounds generate implicit parameters (part of the template) with types
          // from tparams: we need to ensure these don't overlap
          if (!classContextBounds.isEmpty)
            ensureNonOverlapping(template, tparams)
          result
        }
      }
    }

    /** {{{
     *  ObjectDef       ::= Id ClassTemplateOpt
     *  }}}
     */
    def objectDef(start: Offset, mods: Modifiers): ModuleDef = {
      in.nextToken()
      val nameOffset = in.offset
      val name       = ident()
      val tstart     = in.offset
      atPos(start, if (name == nme.ERROR) start else nameOffset) {
        val mods1    = if (in.token == SUBTYPE) mods | Flags.DEFERRED else mods
        val template = templateOpt(mods1, name, NoMods, Nil, tstart)
        ModuleDef(mods1, name.toTermName, template)
      }
    }

    /** Create a tree representing a package object, converting
     *  {{{
     *    package object foo { ... }
     *  }}}
     *  to
     *  {{{
     *    package foo {
     *      object `package` { ... }
     *    }
     *  }}}
     */
    def packageObjectDef(start: Offset): PackageDef = {
      val defn   = objectDef(in.offset, NoMods)
      val pidPos = o2p(defn.pos.startOrPoint)
      val pkgPos = r2p(start, pidPos.point)
      gen.mkPackageObject(defn, pidPos, pkgPos)
    }
    def packageOrPackageObject(start: Offset): Tree = (
      if (in.token == OBJECT)
        joinComment(packageObjectDef(start) :: Nil).head
      else {
        in.flushDoc
        makePackaging(start, pkgQualId(), inBracesOrNil(topStatSeq()))
      }
    )
    // TODO - eliminate this and use "def packageObjectDef" (see call site of this
    // method for small elaboration.)
    def makePackageObject(start: Offset, objDef: ModuleDef): PackageDef = objDef match {
      case ModuleDef(mods, name, impl) =>
        makePackaging(
          start, atPos(o2p(objDef.pos.start)){ Ident(name) }, List(ModuleDef(mods, nme.PACKAGEkw, impl)))
    }

    /** {{{
     *  ClassParents       ::= AnnotType {`(' [Exprs] `)'} {with AnnotType}
     *  TraitParents       ::= AnnotType {with AnnotType}
     *  }}}
     */
    def templateParents(): List[Tree] = {
      val parents  = new ListBuffer[Tree]
      def readAppliedParent() = {
        val start  = in.offset
        val parent = startAnnotType()
        parents += (in.token match {
          case LPAREN => atPos(start)((parent /: multipleArgumentExprs())(Apply.apply))
          case _      => parent
        })
      }
      readAppliedParent()
      while (in.token == WITH) {in.nextToken(); readAppliedParent()}
      parents.toList
    }

    /** {{{
     *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
     *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
     *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
     *  EarlyDef      ::= Annotations Modifiers PatDef
     *  }}}
     */
    def template(): (List[Tree], ValDef, List[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        // @S: pre template body cannot stub like post body can!
        val (self, body) = templateBody(isPre = true)
        if (in.token == WITH && (self eq noSelfType)) {
          val earlyDefs: List[Tree] = body.map(ensureEarlyDef).filter(_.nonEmpty)
          in.nextToken()
          val parents = templateParents()
          val (self1, body1) = templateBodyOpt(parenMeansSyntaxError = false)
          (parents, self1, earlyDefs ::: body1)
        } else {
          (List(), self, body)
        }
      } else {
        val parents = templateParents()
        val (self, body) = templateBodyOpt(parenMeansSyntaxError = false)
        (parents, self, body)
      }
    }

    def ensureEarlyDef(tree: Tree): Tree = tree match {
      case vdef @ ValDef(mods, _, _, _) if !mods.isDeferred =>
        copyValDef(vdef)(mods = mods | Flags.PRESUPER)
      case tdef @ TypeDef(mods, name, tparams, rhs) =>
        deprecationWarning(tdef.pos.point, "early type members are deprecated. Move them to the regular body: the semantics are the same.")
        treeCopy.TypeDef(tdef, mods | Flags.PRESUPER, name, tparams, rhs)
      case docdef @ DocDef(comm, rhs) =>
        treeCopy.DocDef(docdef, comm, rhs)
      case stat if !stat.isEmpty =>
        syntaxError(stat.pos, "only concrete field definitions allowed in early object initialization section", skipIt = false)
        EmptyTree
      case _ =>
        EmptyTree
    }

    /** {{{
     *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
     *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
     *  TraitExtends     ::= `extends' | `<:'
     *  }}}
     */
    def templateOpt(mods: Modifiers, name: Name, constrMods: Modifiers, vparamss: List[List[ValDef]], tstart: Offset): Template = {
      val (parents, self, body) = (
        if (in.token == EXTENDS || in.token == SUBTYPE && mods.isTrait) {
          in.nextToken()
          template()
        }
        else {
          newLineOptWhenFollowedBy(LBRACE)
          val (self, body) = templateBodyOpt(parenMeansSyntaxError = mods.isTrait || name.isTermName)
          (List(), self, body)
        }
      )
      def anyvalConstructor() = (
        // Not a well-formed constructor, has to be finished later - see note
        // regarding AnyVal constructor in AddInterfaces.
        DefDef(NoMods, nme.CONSTRUCTOR, Nil, ListOfNil, TypeTree(), Block(Nil, literalUnit))
      )
      val parentPos = o2p(in.offset)
      val tstart1 = if (body.isEmpty && in.lastOffset < tstart) in.lastOffset else tstart

      atPos(tstart1) {
        // Exclude only the 9 primitives plus AnyVal.
        if (inScalaRootPackage && ScalaValueClassNames.contains(name))
             Template(parents, self, anyvalConstructor :: body)
        else gen.mkTemplate(gen.mkParents(mods, parents, parentPos), self, constrMods, vparamss, body, o2p(tstart))
      }
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** {{{
     *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateBody(isPre: Boolean) = inBraces(templateStatSeq(isPre = isPre)) match {
      case (self, Nil)  => (self, EmptyTree.asList)
      case result       => result
    }
    def templateBodyOpt(parenMeansSyntaxError: Boolean): (ValDef, List[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        templateBody(isPre = false)
      } else {
        if (in.token == LPAREN) {
          if (parenMeansSyntaxError) syntaxError(s"traits or objects may not have parameters", skipIt = true)
          else abort("unexpected opening parenthesis")
        }
        (noSelfType, List())
      }
    }

    /** {{{
     *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
     *  }}}
     */
    def refinement(): List[Tree] = inBraces(refineStatSeq())

/* -------- STATSEQS ------------------------------------------- */

  /** Create a tree representing a packaging. */
    def makePackaging(start: Offset, pkg: Tree, stats: List[Tree]): PackageDef = pkg match {
      case x: RefTree => atPos(start, pkg.pos.point)(PackageDef(x, stats))
    }

    def makeEmptyPackage(start: Offset, stats: List[Tree]): PackageDef = (
      makePackaging(start, atPos(start, start, start)(Ident(nme.EMPTY_PACKAGE_NAME)), stats)
    )

    def statSeq(stat: PartialFunction[Token, List[Tree]], errorMsg: String = "illegal start of definition"): List[Tree] = {
      val stats = new ListBuffer[Tree]
      def default(tok: Token) =
        if (isStatSep) Nil
        else syntaxErrorOrIncompleteAnd(errorMsg, skipIt = true)(Nil)
      while (!isStatSeqEnd) {
        stats ++= stat.applyOrElse(in.token, default)
        acceptStatSepOpt()
      }
      stats.toList
    }

    /** {{{
     *  TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | package object objectDef
     *            | Import
     *            |
     *  }}}
     */
    def topStatSeq(): List[Tree] = statSeq(topStat, errorMsg = "expected class or object definition")
    def topStat: PartialFunction[Token, List[Tree]] = {
      case PACKAGE  =>
        packageOrPackageObject(in.skipToken()) :: Nil
      case IMPORT =>
        in.flushDoc
        importClause()
      case _ if isAnnotation || isTemplateIntro || isModifier =>
        joinComment(topLevelTmplDef :: Nil)
    }

    /** {{{
     *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateStatSeq(isPre : Boolean): (ValDef, List[Tree]) = checkNoEscapingPlaceholders {
      var self: ValDef = noSelfType
      var firstOpt: Option[Tree] = None
      if (isExprIntro) {
        in.flushDoc
        val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(tpnme.EMPTY), tpt) =>
              self = atPos(tree.pos union tpt.pos) { makeSelfDef(nme.WILDCARD, tpt) }
            case _ =>
              convertToParam(first) match {
                case tree @ ValDef(_, name, tpt, EmptyTree) if (name != nme.ERROR) =>
                  self = atPos(tree.pos union tpt.pos) { makeSelfDef(name, tpt) }
                case _ =>
              }
          }
          in.nextToken()
        } else {
          firstOpt = Some(first)
          acceptStatSepOpt()
        }
      }
      (self, firstOpt ++: templateStats())
    }

    /** {{{
     *  TemplateStats    ::= TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
     *                     | Annotations Modifiers Def
     *                     | Annotations Modifiers Dcl
     *                     | Expr1
     *                     | super ArgumentExprs {ArgumentExprs}
     *                     |
     *  }}}
     */
    def templateStats(): List[Tree] = statSeq(templateStat)
    def templateStat: PartialFunction[Token, List[Tree]] = {
      case IMPORT =>
        in.flushDoc
        importClause()
      case _ if isDefIntro || isModifier || isAnnotation =>
        joinComment(nonLocalDefOrDcl)
      case _ if isExprIntro =>
        in.flushDoc
        statement(InTemplate) :: Nil
    }

    def templateOrTopStatSeq(): List[Tree] = statSeq(templateStat.orElse(topStat))

    /** {{{
     *  RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     | type TypeDef
     *                     |
     *  }}}
     */
    def refineStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        stats ++= refineStat()
        if (in.token != RBRACE) acceptStatSep()
      }
      stats.toList
    }

    def refineStat(): List[Tree] =
      if (isDclIntro) { // don't IDE hook
        joinComment(defOrDcl(in.offset, NoMods))
      } else if (!isStatSep) {
        syntaxErrorOrIncomplete(
          "illegal start of declaration"+
          (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
           else ""), skipIt = true)
        Nil
      } else Nil

    /** overridable IDE hook for local definitions of blockStatSeq
     *  Here's an idea how to fill in start and end positions.
    def localDef : List[Tree] = {
      atEndPos {
        atStartPos(in.offset) {
          val annots = annotations(skipNewLines = true)
          val mods = localModifiers() withAnnotations annots
          if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
          else List(tmplDef(mods))
        }
      } (in.offset)
    }
    */

    def localDef(implicitMod: Int): List[Tree] = {
      val annots = annotations(skipNewLines = true)
      val pos  = in.offset
      val mods = (localModifiers() | implicitMod.toLong) withAnnotations annots
      val defs = if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(pos, mods)
                 else List(tmplDef(pos, mods))

      in.token match {
        case RBRACE | CASE  => defs :+ setInPos(literalUnit)
        case _              => defs
      }
    }
    
    /** {{{
     *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     *  }}}
     */
    def blockStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd && !isCaseDefEnd) {
        if (in.token == IMPORT) {
          stats ++= importClause()
          acceptStatSepOpt()
        }
        else if (isDefIntro || isLocalModifier || isAnnotation) {
          if (in.token == IMPLICIT) {
            val start = in.skipToken()
            if (isIdent) stats += implicitClosure(start, InBlock)
            else stats ++= localDef(Flags.IMPLICIT)
          } else {
            stats ++= localDef(0)
          }
          acceptStatSepOpt()
        }
        else if (isExprIntro) {
          stats += statement(InBlock)
          if (!isTokenAClosingBrace(in.token) 
          &&  !isCaseDefEnd                  ) acceptStatSep()
        }
        else if (isStatSep) {
          in.nextToken()
        }
        else {
          val addendum = if (isModifier) " (no modifiers allowed here)" else ""
          syntaxErrorOrIncomplete("illegal start of statement" + addendum, skipIt = true)
        }
      }
      stats.toList
    }

    /** {{{
     *  CompilationUnit ::= {package QualId semi} TopStatSeq
     *  }}}
     */
    def compilationUnit(): PackageDef = checkNoEscapingPlaceholders {
      def topstats(): List[Tree] = {
        val ts = new ListBuffer[Tree]
        while (in.token == SEMI) in.nextToken()
        val start = in.offset
        if (in.token == PACKAGE) {
          in.nextToken()
          if (in.token == OBJECT) {
            // TODO - this next line is supposed to be
            //    ts += packageObjectDef(start)
            // but this broke a scaladoc test (run/diagrams-filtering.scala) somehow.
            ts ++= joinComment(List(makePackageObject(start, objectDef(in.offset, NoMods))))
            if (in.token != EOF) {
              acceptStatSep()
              ts ++= topStatSeq()
            }
          } else {
            in.flushDoc
            val pkg = pkgQualId()

            if (in.token == EOF) {                 ts  +=          makePackaging(start, pkg, List      ())}
            else if (isStatSep)  {in.nextToken (); ts  +=          makePackaging(start, pkg, topstats  ())}
            else {                                 ts  += inBraces(makePackaging(start, pkg, topStatSeq())); acceptStatSepOpt()
                                                   ts ++=                                    topStatSeq()
            }
          }
        } else {                                   ts ++=                                    topStatSeq()}
        ts.toList
      }

      resetPackage()
      topstats() match {
        case (stat @ PackageDef(_, _)) :: Nil => stat
        case stats                            =>
          val start =
            if (stats forall (_ == EmptyTree)) 0
            else {
              val wpos = wrappingPos(stats)
              if (wpos.isDefined) wpos.start
              else 0
            }

          makeEmptyPackage(start, stats)
      }
    }
    
    /**
     * This object contains various transformations of the trees. The resulting trees,
     * as a rule, have some interesting properties from typing point of view, so that
     * you can generate trees that will be typed and treated properly.
     */
    object TypeOperations {
      
      /**
       * This is roughly type casting in pre-typer phase.
       * Wraps the `tree` in a block of following contents:
       * {
       *   val typedReturn: `ttype` = `tree`
       *   typedReturn
       * }
       * 
       * This gives a guarantee, that a) this tree can be assigned to
       * a variable of a given type and b) you actually get a tree of the
       * desired type.
       * 
       * @param ttype - desired type
       * @param tree - tree to be 'casted'
       */
      def enforcingType(ttype: Tree)(tree: Tree): Tree = {
        val typeSafetyDefinition = ValDef(Modifiers(0), newTermName("typedReturn"), ttype, tree)
        Block(typeSafetyDefinition, Ident("typedReturn"))
      }
      
      def withTypeOf(target: Tree)(identToTree: Ident => Tree): Tree =
        withTypeOf(target, Ident(newTypeName("T")))(identToTree)   // TBD: come up with a way to generate unique names for Idents
        
      /**
       * This transformation allows you to capture the type of a `target` value, encapsulate it
       * into the `typePlaceholder` identifier and generate a tree with this type using the Ident => Tree
       * function. The argument that will be passed to this function is the captured type.
       * 
       * Assume this block `block` is given:
       * {
       *   f[T]("Hello, World")
       * }
       * where f is some function and T is unknown type.
       * 
       * Assume some value tree `target` is given, and there's a need to substitute all
       * unknown parameters T in the block with the type of `target`, that is not known either (and
       * will not be known till infered by the compiler on later stages).
       * 
       * Then, withTypeOf(target, Ident(newTypeName("T")))(block) will generate following AST block:
       * {
       *   def capturingFunction[T](x: T) = {
       *     f[T]("Hello, World")
       *   }
       *   capturingFunction(target)
       * }
       */  
      def withTypeOf(target: Tree, typePlaceholder: Ident)(identToTree: Ident => Tree): Tree = {
        import scala.reflect.internal.ModifierFlags._
    
        val tree = identToTree(typePlaceholder)
        
        // Generating a type-capturing function (DefDef)
        val mods = NoMods
        val name = newTermName("capturingFunction")
        
        val typeParam =
          TypeDef(
            Modifiers(DEFERRED | PARAM),
            typePlaceholder.name.asInstanceOf[TypeName],
            List(),
            TypeBoundsTree(EmptyTree, EmptyTree)
          )
              
        val valueParam =
          ValDef(
            Modifiers(BYNAMEPARAM | PARAM),    // TBD: BYNAMEPARAM doesn't seem to actually make it 'by name'
            newTermName("x"),
            typePlaceholder,
            EmptyTree
          )
        val returnType = TypeTree()
        
        val capturingFunction = DefDef(mods, name, List(typeParam), List(List(valueParam)), returnType, tree)
        
        
        // Constructing function application to the target tree
        val application = Apply(Ident(name), List(target))
        
        
        // Returning a block with the capturing function and it's application
        Block(capturingFunction, application)
      }
      
    }
    
  }
}
