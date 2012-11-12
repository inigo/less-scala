package net.surguy.less

import org.specs2.mutable.Specification
import io.Source
import org.specs2.matcher.ParserMatchers
import java.io.File

class CompareJavaScriptToScalaTest extends Specification with ParserMatchers {
  val parsers = LessParser

  private def getFile(name: String) = new File(this.getClass.getResource("/" + name).getFile.replaceAll("%20", " "))
  private def getJsCss(name: String) = adjustWhitespace( JavaScriptLessCompiler.compile(getFile(name))._1 )

  private def read(name: String) = Source.fromInputStream( this.getClass.getResourceAsStream("/" + name) ).getLines().mkString("\n").trim
  private def getScalaCss(name: String) = LessParser.parse(read(name)) match {
    case LessParser.Success(s: Css, next) => adjustWhitespace(CssGenerator.output( s ))
    case fail  => throw new RuntimeException("Could not parse "+name+" due to  "+fail)
  }

  private def adjustWhitespace(s: String) = s.replaceAll("[\r\n ]+", " ").trim

  "Parsing a file with the Less JavaScript parser" should {
    "produce CSS output for a simple CSS file" in { (getJsCss("minimal.less") must not).beNull }
    "produce CSS output for a simple Less file" in { (getJsCss("simpleLess.less") must not).beNull }
  }

  "Processing the same file with the JavaScript and the Scala compilers" should {
    "create the same output for minimal CSS" in {  getScalaCss("minimal.less") mustEqual getJsCss("minimal.less")}
    "create the same output for simple CSS" in {  getScalaCss("simple.less") mustEqual getJsCss("simple.less")}
  }

  "Parsing existing CSS" should {
    getFile("css").listFiles().foreach { f =>
      ("successfully parse "+f.getName ) in { (getScalaCss("css/" + f.getName) must not).beNull }
    }
  }

  "Using JavaScript to parse Less" should {
    getFile("less").listFiles().filter(_.isFile).foreach { f =>
      ("successfully parse "+f.getName ) in { (getJsCss("less/"+f.getName) must not).beNull }
    }
  }

  "Converting existing Less to CSS" should {
    getFile("less").listFiles().filter(_.isFile).foreach { f =>
      ("create the same output from JavaScript and Scala for  "+f.getName ) in {getScalaCss("less/"+f.getName) mustEqual getJsCss("less/"+f.getName)}
    }
  }

}
