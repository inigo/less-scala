package net.surguy.less

import org.specs2.mutable.Specification
import io.Source
import org.specs2.matcher.ParserMatchers
import java.io.File

class CompareJavaScriptToScalaTest extends Specification with ParserMatchers {
  val parsers = LessParser

  private def read(name: String) = Source.fromInputStream( this.getClass.getResourceAsStream("/" + name) ).getLines().mkString("\n").trim
  private def getFile(name: String) = new File(this.getClass.getResource("/" + name).getFile.replaceAll("%20", " "))
  private def getCss(name: String) = JavaScriptLessCompiler.compile(getFile(name))._1

  "Parsing a file with the Less JavaScript parser" should {
    "produce CSS output for a simple CSS file" in { (getCss("minimal.less") must not).beNull }
    "produce CSS output for a simple Less file" in { (getCss("simpleLess.less") must not).beNull }
  }

//  "Processing the same file with the JavaScript and the Scala compilers" should {
//    "create the same output" in { LessParser.parse(read("minimal.less")) mustEqual getCss("minimal.less")}
//  }

}
