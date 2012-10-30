package net.surguy.less

import org.specs2.mutable.Specification
import io.Source
import org.specs2.matcher.ParserMatchers
import LessParser._

class LessParserTest extends Specification with ParserMatchers {
  val parsers = LessParser

  private def read(name: String) = Source.fromInputStream( this.getClass.getResourceAsStream("/" + name) ).getLines().mkString("\n").trim

  "Parsing selectors" should {
    "recognize ids" in { selector("#someId") must beASuccess }
    "recognize classes" in { selector(".someClass") must beASuccess }
    "recognize stars" in { selector("*") must beASuccess }
    "recognize pseudo-classes" in { selector("a:hover") must beASuccess }
    "recognize pseudo-selectors" in { selector("tr:nth-child(3)") must beASuccess }
    "recognize attributes" in { selector("a[title]") must beASuccess }
    "recognize attribute values with apostrophes" in { selector("a[alt='']") must beASuccess }
    "recognize attribute values with quotes" in { selector("a[alt=\"\"]") must beASuccess }
    "recognize attribute values containing" in { selector("a[href*='example.com']") must beASuccess }
    "recognize attribute values beginning" in { selector("a[href^='http']") must beASuccess }
    "recognize attribute values ending" in { selector("a[href$='.jpg']") must beASuccess }
    "recognize attribute values with token" in { selector("a[title~='some']") must beASuccess }
    "chain multiple classes" in { selector("a.btn.important") must beASuccess }
    "support descendants" in { selector("body table a") must beASuccess }
    "support adjacent" in { selector("td + td") must beASuccess }
    "support siblings" in { selector("td ~ td") must beASuccess }
    "support children" in { selector("div > a") must beASuccess }
    "support negation" in { selector("div:not(#container)") must beASuccess }
  }

  "Parsing declarations" should {
    "recognize simple declarations" in { declaration must succeedOn("font-weight: bold") }
    "recognize important declarations" in { declaration must succeedOn("font-weight: bold !important") }
    "recognize colours" in { declaration must succeedOn("color: red") }
    "recognize hex colours" in { declaration must succeedOn("color: #f0f0f0") }
    "recognize hex colour triples" in { declaration must succeedOn("color: #fff") }
    "recognize rgb colours" in { declaration must succeedOn("color: rgb(120,120,120)") }
    "recognize multiple values" in { declaration must succeedOn("font-family: 'Times New Roman', sans-serif") }
    "recognize urls" in { declaration must succeedOn("background-image: url('http://example.com/test.jpg?asd#value')") }
    "recognize counters" in { declaration must succeedOn("content: counter(chapter) \".\" counter(section) \" \";") }
  }

  "Parsing rulesets" should {
    "recognize simple rules" in { ruleset must succeedOn("a { font-weight: bold; }") }
    "recognize rules without semi-colons" in { ruleset must succeedOn("a { font-weight: bold }") }
    "recognize multiple declarations" in { ruleset must succeedOn("a { font-weight: bold; text-decoration: underline; }") }
  }

  "Parsing directives" should {
    "recognize simple imports" in { directive must succeedOn("@import 'test.css';")  }
    "recognize imports with media queries" in { directive must succeedOn("@import 'test.css' projection,tv;")  }
    "not consume rules" in { directive must failOn("a { font-weight: bold; }")  }
  }

  "Parsing complete CSS files" should {
    "work for minimal CSS" in { stylesheet(read("minimal.less")) must beASuccess }
    "work for simple CSS" in { stylesheet(read("simple.less")) must beASuccess }
  }

  "Parsing comments" should {
    "recognize standard CSS comments" in { comment must succeedOn("/* Some comment */") }
  }

  "Parsing colours" should {
    "parse colours in rgb format" in { color must succeedOn("rgb(10,20,30)").withResult(RgbColor("10","20","30")) }
    "parse colours in rgba format" in { color must succeedOn("rgba(10,20,30, 40)").withResult(RgbaColor("10","20","30","40")) }
    "parse colours in hash format" in { color must succeedOn("#f0f0f0").withResult(HashColor("f0f0f0")) }
    "parse colours in short hash format" in { color must succeedOn("#fff").withResult(HashColor("fff")) }
    "parse colours in named format" in { color must succeedOn("green").withResult(NamedColor("green")) }
  }

  /* ----- Less-specific tests ------ */

  "Parsing variables" should {
    "recognize simple variables" in { variable must succeedOn("@nice-blue: #5B83AD;")  }
    "recognize variables with + operator" in { variable must succeedOn("@light-blue: (@nice-blue + #111);")  }
    "recognize variables with - operator" in { variable must succeedOn("@light-blue: (@nice-blue - #111);")  }
    "recognize variables with * operator" in { variable must succeedOn("@light-blue: (@base * 2);")  }
    "recognize variables with / operator" in { variable must succeedOn("@light-blue: (@base / 2);")  }
    "recognize variables with brackets" in { variable must succeedOn("@light-blue: ((@base / 2) * 2);")  }
    "recognize variables with units" in { variable must succeedOn("@var: (1px + 5);")  }

    "recognize variable references" in { ruleset must succeedOn("#header { color: @light-blue; }") }
    "recognize variable variable references" in { ruleset must succeedOn("#header { color: @@someColor; }") }
    "recognize variables in selectors" in { ruleset must succeedOn(".@{someVar} { color: black; }") }
  }

  "Parsing mixins" should {
    "allow inclusion of simple mixins" in { ruleset must succeedOn("table { .tbl; }") }
    "allow inclusion of mixins and rules together" in { ruleset must succeedOn("table { .tbl; font-weight: bold; }") }
    "allow inclusion of parametric mixins" in { ruleset must succeedOn("table { .tbl(4px); }") }
  }

  "Guard expressions" should {
    "work with conditionals" in { ruleset must succeedOn(".mixin (@a) when (lightness(@a) >= 50%) { color: black;}") }
    "work without a conditional" in { ruleset must succeedOn(".truth (@a) when (@a) { color: red; }") }
  }

}
