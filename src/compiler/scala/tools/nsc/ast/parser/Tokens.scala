/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast.parser

object Tokens extends CommonTokens {
  final val STRINGPART = 7 // a part of an interpolated string
  final val SYMBOLLIT = 8
  final val INTERPOLATIONID = 9 // the lead identifier of an interpolated string

  def isLiteral(code: Int) = code >= CHARLIT && code <= INTERPOLATIONID

  /** identifiers */
  final val IDENTIFIER = 10
  final val BACKQUOTED_IDENT = 11
  def isIdentifier(code: Int) = code == IDENTIFIER || code == BACKQUOTED_IDENT // used by ide

  /** modifiers */
  final val IMPLICIT = 40
  final val OVERRIDE = 41
  final val SEALED = 45
  final val LAZY = 55
  final val MACRO = 57

  /** templates */
  final val CASECLASS = 63
  final val OBJECT = 64
  final val CASEOBJECT = 65
  final val TRAIT = 66
  final val WITH = 69
  final val TYPE = 70
  final val FORSOME = 71
  final val DEF = 72
  final val VAL = 73
  final val VAR = 74

  /** control structures */
  final val THEN = 81
  final val YIELD = 86
  final val MATCH = 95

  /** special symbols */
  final val HASH = 130
  final val USCORE = 131
  final val ARROW = 132
  final val LARROW = 133
  final val SUBTYPE = 134
  final val SUPERTYPE = 135
  final val VIEWBOUND = 136
  final val NEWLINE = 137
  final val NEWLINES = 138
  final val XMLSTART = 139

  /** for IDE only */
  final val COMMENT = 200
  final val WHITESPACE = 201
  final val IGNORE = 202
  final val ESCAPE = 203
  
  /** SubScript */
  final val SCRIPT                   = 330
  final val IF_QMARK                 = 331
  final val DOT2                     = 332
  final val DOT3                     = 333
  final val LESS2                    = 334
  final val GREATER2                 = 335
  final val ARROW2                   = 336
  final val CURLYARROW2              = 337
  final val CURLYBROKENARROW2        = 338
  
  final val LBRACE_DOT               = 340
  final val LBRACE_DOT3              = 341
  final val LBRACE_QMARK             = 342
  final val LBRACE_EMARK             = 343
  final val LBRACE_ASTERISK          = 344
  final val LBRACE_CARET             = 345
                                     
  final val RBRACE_DOT               = 351
  final val RBRACE_DOT3              = 352
  final val RBRACE_QMARK             = 353
  final val RBRACE_EMARK             = 354
  final val RBRACE_ASTERISK          = 355
  final val RBRACE_CARET             = 356
                                     
  final val LPAREN_PLUS_RPAREN       = 360
  final val LPAREN_MINUS_RPAREN      = 361
  final val LPAREN_PLUS_MINUS_RPAREN = 362
  final val LPAREN_SEMI_RPAREN       = 363

  final val LPAREN_ASTERISK          = 364
  final val LPAREN_ASTERISK2         = 365
  final val RPAREN_ASTERISK          = 366
  final val RPAREN_ASTERISK2         = 367

  final val DO_THEN                  = 370 // only for generating code
  final val DO_ELSE                  = 371 // only for generating code
  final val DO_THEN_ELSE             = 372 // only for generating code
  
  final val scriptBracePairs = Map (
    LBRACE          -> RBRACE         ,
    LBRACE_DOT      -> RBRACE_DOT     ,
    LBRACE_DOT3     -> RBRACE_DOT3    ,  
    LBRACE_QMARK    -> RBRACE_QMARK   ,  
    LBRACE_EMARK    -> RBRACE_EMARK   ,  
    LBRACE_ASTERISK -> RBRACE_ASTERISK,  
    LBRACE_CARET    -> RBRACE_CARET   ) 

}
