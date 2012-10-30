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
    case Declaration(property, value) => property.text + ": " + value.value.trim + "; "

    case RgbColor(r, g, b) => "rgb(%s,%s,%s)".format(r, g, b)
    case RgbaColor(r, g, b, a) => "rgba(%s,%s,%s,%s)".format(r, g, b, a)
    case NamedColor(name) => toRgb(Colors.values(name).substring(1))
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

}
