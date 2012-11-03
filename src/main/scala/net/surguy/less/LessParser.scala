package net.surguy.less

import util.parsing.combinator.RegexParsers

/**
 * Parse a LESS stylesheet - see http://lesscss.org/.
 *
 * LESS is a superset of CSS, so this will also parse CSS.
 */
object LessParser extends RegexParsers {

  def parse(lessText : String):ParseResult[Stylesheet] = parseAll(stylesheet, lessText)

  // @todo Is the comment match only working due to type erasure?
  def stylesheet : Parser[Stylesheet] = rep( (comment | directive) ) ~ rep( (comment | ruleset) ) ^^
    { case (directives: Seq[Directive]) ~ (rules : Seq[Ruleset]) => Stylesheet(directives, rules) }

  def directive: Parser[Directive] = "@" ~> directiveTerm <~ ";" ^^
    { case directive: DirectiveTerm => Directive(directive) }

  def directiveTerm: Parser[DirectiveTerm] = "[^;]+".r ^^ { s => DirectiveTerm(s) }

  // @todo Is the comment match only working due to type erasure?
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

  def variable: Parser[Variable] = variableName ~ ":" ~ variableValue <~ ";" ^^
    { case (name: VariableName) ~ ":" ~ (value: VariableValue) => Variable(name, value)   }
  def variableValue: Parser[VariableValue] = variableOperation | variableName | variableSimpleValue
  def variableName: Parser[VariableName] = "@[\\w-]+".r ^^ { s => VariableName(s) }
  def variableSimpleValue: Parser[VariableSimpleValue] = "[\\w\\-#]+".r ^^ { s => VariableSimpleValue(s) }
  def variableOperation: Parser[VariableOperation] = "(" ~> variableValue ~ operator ~ variableValue <~ ")" ^^
    { case (value1: VariableValue) ~ (op: Operator) ~ (value2: VariableValue) => VariableOperation(value1, op, value2) }
  def operator: Parser[Operator] = "[+*/\\-]".r ^^ { s => Operator(s) }

  def color: Parser[Color] = (rgbaColor | rgbColor | hashColor | namedColor) ^^ { case (color: Color) => color }

  def rgbColor: Parser[RgbColor] = "rgb(" ~> "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r <~ ")" ^^
    { case (r: String) ~ "," ~ (g: String) ~ "," ~ (b:String)  => RgbColor(r, g, b)  }
  def rgbaColor: Parser[RgbaColor] = "rgba(" ~> "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r ~ "," ~ "\\w+".r <~ ")" ^^
    { case (r: String) ~ "," ~ (g: String) ~ "," ~ (b:String) ~ "," ~ (a:String) => RgbaColor(r, g, b, a)  }
  def hashColor: Parser[HashColor] = "#" ~> "\\w+".r ^^ { s => HashColor(s) }
  def namedColor: Parser[NamedColor] = ("("+Colors.names.mkString("|")+")").r ^^ { s => NamedColor(s) }
}

sealed abstract class Css
case class Stylesheet(directives: Seq[Directive], rules: Seq[Ruleset]) extends Css
case class Directive(directive: DirectiveTerm) extends Css
case class Ruleset(selector: Selector, declarations: Seq[Declaration]) extends Css
case class Selector(terms: Seq[SelectorTerm]) extends Css
case class Declaration(property: Property, value: Value) extends Css
case class SelectorTerm(text: String) extends Css
case class Property(text: String) extends Css
case class DirectiveTerm(text: String) extends Css
case class Comment(text: String) extends Css

case class Variable(name: VariableName, value: VariableValue) extends Css
case class VariableName(name: String) extends VariableValue
// @todo Probably the same as other values - e.g. for properties
sealed abstract class VariableValue extends Css
case class VariableSimpleValue(name: String) extends VariableValue
case class Operator(op: String) extends Css
case class VariableOperation(val1: VariableValue, op: Operator, val2: VariableValue) extends VariableValue

sealed abstract class Value extends Css
case class SimpleValue(value: String) extends Value

sealed abstract class Color extends Value
case class RgbColor(r: String, g: String, b: String) extends Color
case class RgbaColor(r: String, g: String, b: String, a: String) extends Color
case class HashColor(value: String) extends Color
case class NamedColor(name: String) extends Color

case object NullCss extends Css