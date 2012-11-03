package net.surguy.less

/**
 * Convert CSS objects into text form.
 *
 * @author Inigo Surguy
 */
object CssGenerator {

  def output(css: Css): String = css match {
    case Stylesheet(directives, rules) => directives.map(output).mkString("\n") + rules.map(output).mkString("\n")
    case Directive(directive) => directive.text
    case Ruleset(selector, declarations) => output(selector) + " { " + declarations.map(output).mkString + "}"
    case Selector(terms) => terms.map(_.text).mkString(" ")
    case Declaration(property, value) => property.text + ": " + output(value) + "; "
    case NullCss => ""

    case SimpleValue(v) => v.trim
//    case RgbColor(r, g, b) => "rgb(%s,%s,%s)".format(r, g, b)
//    case RgbaColor(r, g, b, a) => "rgba(%s,%s,%s,%s)".format(r, g, b, a)
    case col: RgbColor => toHashColor(col)
    case col: RgbaColor => toHashColor(col)
    case NamedColor(name) => name
    case HashColor(value) => toRgb(value)
  }

  def toRgb(hash: String) = {
    def dec(start: Int, end: Int) = java.lang.Long.parseLong(hash.substring(start, end), 16)
    def decdec(start: Int, end: Int) = java.lang.Long.parseLong(hash.substring(start, end)+hash.substring(start, end), 16)
    hash match {
      case _ if hash.length==3 => "rgb(%s,%s,%s)".format( decdec(0,1), decdec(1,2), decdec(2,3) )
      case _ if hash.length==4 => "rgba(%s,%s,%s,%s)".format( decdec(0,1), decdec(1,2), decdec(2,3), decdec(3,4) )
      case _ if hash.length==6 => "rgb(%s,%s,%s)".format( dec(0,2), dec(2,4), dec(4,6) )
      case _ if hash.length==8 => "rgba(%s,%s,%s,%s)".format( dec(0,2), dec(2,4), dec(4,6), dec(6,8) )
      case _ => "#"+hash
    }
  }

  def toHashColor(col: RgbColor) = "#"+hex(col.r)+hex(col.g)+hex(col.b)
  def toHashColor(col: RgbaColor) = "#"+hex(col.r)+hex(col.g)+hex(col.b)+hex(col.a)
  private def hex(s: String) = "%02X".format(s.toInt).toLowerCase // Lowercase because that's what Less.js does

}
