package net.surguy.less

import org.specs2.mutable.Specification

/**
 * @author Inigo Surguy
 */
class LessProcessorTest extends Specification {
  import LessProcessor._

  "Processing a CSS tree with no Less" should {
    val simpleRuleset = new Ruleset(new Selector(List(SelectorTerm(".thing"))), List(new Declaration(new Property("color"), new SimpleValue("red"))))
    "give the same output when there are no transformations" in { process(simpleRuleset) mustEqual simpleRuleset}
    "use the partial function to transform specific values" in {
      process(simpleRuleset, {case SelectorTerm(t) => SelectorTerm("changed") }) mustEqual
        new Ruleset(new Selector(List(SelectorTerm("changed"))), List(new Declaration(new Property("color"), new SimpleValue("red"))))
    }
  }

}
