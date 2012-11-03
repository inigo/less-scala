package net.surguy.less

/**
 * Apply Less rules to the parse tree, transforming it into a CSS parse tree.
 *
 * @author Inigo Surguy
 */
object LessProcessor {
  private val NullPf: PartialFunction[Css, Css] = { case x if false => x }

  def process(css: Css, pf: PartialFunction[Css, Css] = NullPf): Css = new Transformer(pf).process(css)

  // @todo Do this with a Lens instead: http://stackoverflow.com/questions/3900307/cleaner-way-to-update-nested-structures/3900498#3900498
  class Transformer(pf: PartialFunction[Css, Css]) {

    val addVariables: PartialFunction[Css, Css] = {
      val variables = collection.mutable.HashMap.empty[String,Css]
      val pf: PartialFunction[Css, Css] = {
        case VariableSimpleValue(value) =>
          VariableSimpleValue(value)
        // @todo Evaluate variables
        case VariableOperation(val1, op, val2) =>
          VariableOperation(val1, op, val2)
        // Store variables to be substituted in later
        case Variable(VariableName(name), value) =>
          variables(name) = process(value)
          NullCss
      }
      pf
    }

    val functions = pf.orElse(addVariables)

    def process(css: Css): Css = {
      css match {
        case x if functions.isDefinedAt(x) => functions.apply(x)

        case Stylesheet(directives, rules) => Stylesheet(directives.map(process).asInstanceOf[Seq[Directive]], rules.map(process).asInstanceOf[Seq[Ruleset]] )
        case Directive(directive) => Directive(process(directive).asInstanceOf[DirectiveTerm])
        case Ruleset(selector, declarations) => Ruleset(process(selector).asInstanceOf[Selector], declarations.map(process).asInstanceOf[Seq[Declaration]])
        case Selector(terms) => Selector(terms.map(process).asInstanceOf[Seq[SelectorTerm]])
        case Declaration(property, value) => Declaration(process(property).asInstanceOf[Property], process(value).asInstanceOf[Value])
        case Variable(name, value) => Variable(process(name).asInstanceOf[VariableName], process(value).asInstanceOf[VariableValue])
        case VariableOperation(val1, op, val2) => VariableOperation(process(val1).asInstanceOf[VariableValue], process(op).asInstanceOf[Operator], process(val2).asInstanceOf[VariableValue])
        case terminal => terminal
      }
    }
  }

}
