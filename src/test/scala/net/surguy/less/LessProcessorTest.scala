package net.surguy.less

import org.specs2.mutable.Specification

/**
 * @author Inigo Surguy
 */
class LessProcessorTest extends Specification {
  import LessProcessor._
  import LessParser._
  def less(css: String) = process(parse(css).get)

  "Processing a CSS tree with no Less" should {
    val simpleRuleset = new Ruleset(new Selector(List(SelectorTerm(".thing"))), List(new Declaration(new Property("color"), new SimpleValue("red"))))
    "give the same output when there are no transformations" in { process(simpleRuleset) mustEqual simpleRuleset}
    "use the partial function to transform specific values" in {
      process(simpleRuleset, {case SelectorTerm(t) => SelectorTerm("changed") }) mustEqual
        new Ruleset(new Selector(List(SelectorTerm("changed"))), List(new Declaration(new Property("color"), new SimpleValue("red"))))
    }
  }

  "Processing variables" should {
    "Inline simple variables" in { less("@font: 'Arial'; p { font-family: @font; }") mustEqual parse("p { font-family: 'Arial'; }") }
    "Inline color variables" in { less("@col: #ff0; p { color: @col; }") mustEqual parse("p { color: #ff0; }") }
    "Inline numeric variables" in { less("@size: 10px; p { font-size: @size; }") mustEqual parse("p { font-size: 10px; }") }
    "Scope variables to their blocks" in { less("@col: red; p { @col: white; color: @col; }") mustEqual parse("p { color: white; }") }
    "Interpolate variables in selectors" in { less("@name: blocked; .@{name} { color: red; }") mustEqual parse(".blocked { color: red; }") }
    "Interpolate variables in strings" in { less("@isSans: 'sans-'; p { font: '@{isSans}serif'; )") mustEqual parse("p { font: 'sans-serif'; }") }
  }

  "Processing operators on simple types" should {
    "Add sizes with the same px unit" in { less("p { font-size: ( 10px + 5px ); }") mustEqual parse("p { font-size: 15px; }") }
    "Add sizes with the same % unit" in { less("p { font-size: ( 10% + 5% ); }") mustEqual parse("p { font-size: 15%; }") }
    "Add sizes with no units" in { less("p { font-size: ( 10 + 5 ); }") mustEqual parse("p { font-size: 15; }") }
    "Add sizes with one unit on the first operand" in { less("p { font-size: ( 10px + 5 ); }") mustEqual parse("p { font-size: 15px; }") }
    "Add sizes with one unit on the second operand" in { less("p { font-size: ( 10 + 5px ); }") mustEqual parse("p { font-size: 15px; }") }
    "Not add sizes with mixed units" in { less("p { font-size: ( 10px + 10% ); }") must throwA[RuntimeException] }
    "Not add sizes to colors" in { less("p { font-size: ( 10px + red ); }") must throwA[RuntimeException] }
// Not sure if this requires additional brackets
//    "Add multiple items" in { less("p { font-size: ( 10px + 5px + 3px ); }") mustEqual parse("p { font-size: 18px; }") }
    "Multiply sizes with no units" in { less("p { font-size: ( 10 * 2 ); }") mustEqual parse("p { font-size: 20; }") }
    "Multiply sizes with % units" in { less("p { font-size: ( 10% * 2 ); }") mustEqual parse("p { font-size: 20%; }") }
    "Subtract sizes with the same px unit" in { less("p { font-size: ( 10px - 3px ); }") mustEqual parse("p { font-size: 7px; }") }
    "Subtract sizes with no units" in { less("p { font-size: ( 10 - 3 ); }") mustEqual parse("p { font-size: 7; }") }
    "Divide sizes with no units to integers" in { less("p { font-size: ( 10 / 2 ); }") mustEqual parse("p { font-size: 5; }") }
    "Divide sizes with px units to integers" in { less("p { font-size: ( 10px / 2 ); }") mustEqual parse("p { font-size: 5px; }") }
//    "Divide sizes with no units to floats" in { less("p { font-size: ( 22 / 7 ); }") mustEqual parse("p { font-size: 3.1429; }") }
  }

  "Processing operators on colors" should {
    "Add colors" in { less("p { color: ( #ff0000 + #00ff00); }") mustEqual parse("p { color: #ffff00; }") }
  }

  "Processing functions" should {
    "Support round" in { less("p { font-size: round(1.67); }") mustEqual parse("p { font-size: 2; }") }
    "Support ceil" in { less("p { font-size: ceil(2.4); }") mustEqual parse("p { font-size: 3; }") }
    "Support floor" in { less("p { font-size: floor(2.6); }") mustEqual parse("p { font-size: 2; }") }
    "Support percentage" in { less("p { font-size: percentage(0.5); }") mustEqual parse("p { font-size: 50%; }") }
  }

  "Processing colour functions" should {
    // @todo The colour results here are mostly wrong
    "Support lighten" in { less("p { color: lighten(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support darken" in { less("p { color: darken(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support saturate" in { less("p { color: saturate(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support desaturate" in { less("p { color: desaturate(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support fadein" in { less("p { color: fadein(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support fadeout" in { less("p { color: fadeout(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support fade" in { less("p { color: fade(#909090, 10%); }") mustEqual parse("p { color: #000000; }") }
    "Support positive spin" in { less("p { color: spin(#ff0000, 10); }") mustEqual parse("p { color: #000000; }") }
    "Support negative spin" in { less("p { color: spin(#ff0000, -10); }") mustEqual parse("p { color: #000000; }") }
    "Support mix" in { less("p { color: mix(#ff0000, #00ff00, 50%); }") mustEqual parse("p { color: #000000; }") }
    "Support dark contrast" in { less("p { color: contrast(#101010, #000, #fff); }") mustEqual parse("p { color: #fff; }") }
    "Support light contrast" in { less("p { color: contrast(#e0e0e0, #000, #fff); }") mustEqual parse("p { color: #000; }") }
  }

  "Extracting colour values" should {
    // @todo The colour results here are mostly wrong
    "Support hue" in { less("p { font-family: hue(#909090); }") mustEqual parse("p { font-family: 0; }") }
    "Support saturation" in { less("p { font-family: saturation(#909090); }") mustEqual parse("p { font-family: 0; }") }
    "Support lightness" in { less("p { font-family: lightness(#909090); }") mustEqual parse("p { font-family: 0; }") }
    "Support red" in { less("p { font-family: red(#a0b0c0); }") mustEqual parse("p { font-family: 160; }") }
    "Support green" in { less("p { font-family: green(#a0b0c0); }") mustEqual parse("p { font-family: 160; }") }
    "Support blue" in { less("p { font-family: blue(#a0b0c0); }") mustEqual parse("p { font-family: 176; }") }
    "Support alpha" in { less("p { font-family: alpha(#a0b0c0d0); }") mustEqual parse("p { font-family: 208; }") }
    "Support luma" in { less("p { font-family: luma(#909090); }") mustEqual parse("p { font-family: 0; }") }
  }

//
//  "Processing mixins" should {
//  }
//  "Processing nested selectors" should {
//  }

}
