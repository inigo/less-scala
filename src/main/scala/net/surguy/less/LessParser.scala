package net.surguy.less

import util.parsing.combinator.RegexParsers

/**
 * Parse a LESS stylesheet - see http://lesscss.org/.
 *
 * LESS is a superset of CSS, so this will also parse CSS.
 */
class LessParser extends RegexParsers {

  def parse(lessText : String):ParseResult[Stylesheet] = parseAll(stylesheet, lessText)

  def stylesheet : Parser[Stylesheet] = rep( ruleset ) ^^
    { case rules : Seq[Ruleset] => Stylesheet(rules) }

  def ruleset : Parser[Ruleset] = selector ~ "{" ~ rep(declaration) ~ "}" ^^
    { case (selector: Selector) ~ "{" ~ (declarations : Seq[Declaration]) ~ "}" => Ruleset(selector, declarations) }

  def selector : Parser[Selector] = rep(selectorTerm) ^^
    { case selectorTerms: Seq[SelectorTerm] => Selector(selectorTerms) }

  def selectorTerm : Parser[SelectorTerm] = "[.:#()\\w\\-]+".r ^^ { s => SelectorTerm(s) }

  def declaration: Parser[Declaration] = property ~ ":" ~ value <~ opt(";") ^^
    { case (property: Property) ~ ":" ~ (value: Value)  => Declaration(property, value) }

  def property: Parser[Property] = "[\\w-]+".r ^^ { s => Property(s) }
  def value: Parser[Value] = "[^;}]+".r ^^ { s => Value(s) }

//  stylesheet  : [ CDO | CDC | S | statement ]*;
//  statement   : ruleset | at-rule;
//  at-rule     : ATKEYWORD S* any* [ block | ';' S* ];
//  block       : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
//  ruleset     : selector? '{' S* declaration? [ ';' S* declaration? ]* '}' S*;
//  selector    : any+;
//  declaration : property S* ':' S* value;
//  property    : IDENT;
//  value       : [ any | block | ATKEYWORD S* ]+;
//  any         : [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
//  | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
//  | DASHMATCH | ':' | FUNCTION S* [any|unused]* ')'
//  | '(' S* [any|unused]* ')' | '[' S* [any|unused]* ']'
//  ] S*;
//  unused      : block | ATKEYWORD S* | ';' S* | CDO S* | CDC S*;
}

case class Stylesheet(rules: Seq[Ruleset])
case class Ruleset(selector: Selector, declarations: Seq[Declaration])
case class Selector(terms: Seq[SelectorTerm])
case class Declaration(property: Property, value: Value)
case class Value(value: String)
case class SelectorTerm(text: String)
case class Property(text: String)
