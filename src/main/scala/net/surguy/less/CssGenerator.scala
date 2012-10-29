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
  }

}
