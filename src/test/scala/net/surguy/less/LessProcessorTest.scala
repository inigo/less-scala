package net.surguy.less

import org.specs2.mutable.Specification

/**
 * @author Inigo Surguy
 */
class LessProcessorTest extends Specification {

  import LessProcessor._

  "Processing a CSS tree with no Less" should {
    "give the same output" in {
      val css = new Ruleset(new Selector(List(SelectorTerm(".thing"))), List(new Declaration(new Property("color"), new SimpleValue("red"))))
      process(css) mustEqual css
    }
  }

}
