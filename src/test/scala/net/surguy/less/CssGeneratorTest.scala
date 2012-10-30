package net.surguy.less

import org.specs2.mutable.Specification

/**
 * @author Inigo Surguy
 */
class CssGeneratorTest extends Specification{

  "Converting hashes to colours" should {
    "convert triplets to rgb" in { CssGenerator.toRgb("101010") mustEqual "rgb(16,16,16)" }
    "convert triplets to rgb for min and max values" in { CssGenerator.toRgb("FF00FF") mustEqual "rgb(255,0,255)" }
    "convert quads to rgba" in { CssGenerator.toRgb("10101010") mustEqual "rgba(16,16,16,16)" }
    "convert half triples to rgb" in { CssGenerator.toRgb("fff") mustEqual "rgb(255,255,255)" }
    "convert half quads to rgba" in { CssGenerator.toRgb("ffff") mustEqual "rgba(255,255,255,255)" }
    "retain other hash values" in { CssGenerator.toRgb("12") mustEqual "#12" }
  }

}
