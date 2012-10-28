package net.surguy.less

import org.specs2.mutable.Specification
import io.Source

class LessParserTest extends Specification {

  def parse(name: String) = {
    val lessText = Source.fromInputStream( this.getClass.getResourceAsStream("/" + name) ).getLines().mkString("\n")
    println(lessText)
    val parsed = new LessParser().parse(lessText)
    println(parsed)
    parsed
  }

  "Parsing CSS" should {
    "work for minimal CSS" in { (parse("minimal.less") must not).throwAn[Exception] }
    "work for simple CSS" in { (parse("simple.less") must not).throwAn[Exception] }
  }

}
