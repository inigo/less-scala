package net.surguy.less

import util.parsing.combinator.RegexParsers

/**
 * Parse a LESS stylesheet - see http://lesscss.org/.
 *
 * LESS is a superset of CSS, so this will also parse CSS.
 */
object LessParser extends RegexParsers {

  def parse(lessText : String):ParseResult[Stylesheet] = parseAll(stylesheet, lessText)

  def stylesheet : Parser[Stylesheet] = rep( (comment | directive) ) ~ rep( (comment | ruleset) ) ^^
    { case (directives: Seq[Directive]) ~ (rules : Seq[Ruleset]) => Stylesheet(directives, rules) }

  def directive: Parser[Directive] = "@" ~> directiveTerm <~ ";" ^^
    { case directive: DirectiveTerm => Directive(directive) }

  def directiveTerm: Parser[DirectiveTerm] = "[^;]+".r ^^ { s => DirectiveTerm(s) }

  def ruleset : Parser[Ruleset] = selector ~ "{" ~ rep( (comment | declaration)) ~ "}" ^^
    { case (selector: Selector) ~ "{" ~ (declarations : Seq[Declaration]) ~ "}" => Ruleset(selector, declarations) }

  def selector : Parser[Selector] = rep(selectorTerm) ^^
    { case selectorTerms: Seq[SelectorTerm] => Selector(selectorTerms) }

  def selectorTerm : Parser[SelectorTerm] = "[.:\\[\\]*>~'\"=^$\\+#()\\w\\-]+".r ^^ { s => SelectorTerm(s) }

  def declaration: Parser[Declaration] = property ~ ":" ~ (color | value) <~ opt(";") ^^
    { case (property: Property) ~ ":" ~ (value: Value)  => Declaration(property, value) }

  def property: Parser[Property] = "[\\w-]+".r ^^ { s => Property(s) }
  def value: Parser[SimpleValue] = "[^;}]+".r ^^ { s => SimpleValue(s) }

  def comment: Parser[Comment] = "/*" ~> ".*(?=\\*/)".r <~ "*/" ^^ { s => Comment(s)  }

  def variable: Parser[Variable] = "@" ~> property ~ ":" ~ value <~ ";" ^^
    { case (property: Property) ~ ":" ~ (value: Value) => Variable(property.text, value.value)   }

  def color: Parser[Color] = (rgbaColor | rgbColor | hashColor | namedColor) ^^ { case (color: Color) => color }

  def rgbColor: Parser[RgbColor] = "rgb(" ~> "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r <~ ")" ^^
    { case (r: String) ~ "," ~ (g: String) ~ "," ~ (b:String)  => RgbColor(r, g, b)  }
  def rgbaColor: Parser[RgbaColor] = "rgba(" ~> "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r <~ ")" ^^
    { case (r: String) ~ "," ~ (g: String) ~ "," ~ (b:String) ~ "," ~ (a:String) => RgbaColor(r, g, b, a)  }
  def hashColor: Parser[HashColor] = "#" ~> "\\w+".r ^^ { s => HashColor(s) }
  def namedColor: Parser[NamedColor] = ("("+Colors.names.mkString("|")+")").r ^^ { s => NamedColor(s) }

  //      CSS 2.1 grammar from http://www.w3.org/TR/CSS21/grammar.html
//  stylesheet : [ CHARSET_SYM STRING ';' ]? [S|CDO|CDC]* [ import [ CDO S* | CDC S* ]* ]*
//        [ [ ruleset | media | page ] [ CDO S* | CDC S* ]* ]* ;
//  import : IMPORT_SYM S* [STRING|URI] S* media_list? ';' S* ;
//  media : MEDIA_SYM S* media_list '{' S* ruleset* '}' S* ;
//  media_list : medium [ COMMA S* medium]* ;
//  medium : IDENT S* ;
//  page : PAGE_SYM S* pseudo_page? '{' S* declaration? [ ';' S* declaration? ]* '}' S* ;
//  pseudo_page : ':' IDENT S* ;
//  operator : '/' S* | ',' S* ;
//  combinator : '+' S* | '>' S* ;
//  unary_operator : '-' | '+' ;
//  property : IDENT S* ;
//  ruleset : selector [ ',' S* selector ]* '{' S* declaration? [ ';' S* declaration? ]* '}' S* ;
//  selector : simple_selector [ combinator selector | S+ [ combinator? selector ]? ]? ;
//  simple_selector : element_name [ HASH | class | attrib | pseudo ]* | [ HASH | class | attrib | pseudo ]+ ;
//  class : '.' IDENT ;
//  element_name : IDENT | '*' ;
//  attrib : '[' S* IDENT S* [ [ '=' | INCLUDES | DASHMATCH ] S* [ IDENT | STRING ] S* ]? ']' ;
//  pseudo : ':' [ IDENT | FUNCTION S* [IDENT S*]? ')' ] ;
//  declaration : property ':' S* expr prio? ;
//  prio : IMPORTANT_SYM S* ;
//  expr : term [ operator? term ]* ;
//  term : unary_operator?
//        [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* | TIME S* | FREQ S* ]
//        | STRING S* | IDENT S* | URI S* | hexcolor | function ;
//  function : FUNCTION S* expr ')' S* ;
//  /* There is a constraint on the color that it must have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
//     after the "#"; e.g., "#000" is OK, but "#abcd" is not. */
//  hexcolor : HASH S* ;

}

sealed abstract class Css
case class Stylesheet(directies: Seq[Directive], rules: Seq[Ruleset]) extends Css
case class Directive(directive: DirectiveTerm) extends Css
case class Ruleset(selector: Selector, declarations: Seq[Declaration]) extends Css
case class Selector(terms: Seq[SelectorTerm]) extends Css
case class Declaration(property: Property, value: Value) extends Css
case class SelectorTerm(text: String) extends Css
case class Property(text: String) extends Css
case class DirectiveTerm(text: String) extends Css
case class Comment(text: String) extends Css
case class Variable(name: String, value: String) extends Css

sealed abstract class Value extends Css
case class SimpleValue(value: String) extends Value

sealed abstract class Color extends Value
case class RgbColor(r: String, g: String, b: String) extends Color
case class RgbaColor(r: String, g: String, b: String, a: String) extends Color
case class HashColor(value: String) extends Color
case class NamedColor(name: String) extends Color
