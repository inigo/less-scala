package net.surguy.less

import org.specs2.mutable.Specification

/**
 * @author Inigo Surguy
 */
class CssGeneratorTest extends Specification{

  import CssGenerator._

  "Converting hashes to colours" should {
    "convert triplets to rgb" in { toRgb("101010") mustEqual "rgb(16,16,16)" }
    "convert triplets to rgb for min and max values" in { toRgb("FF00FF") mustEqual "rgb(255,0,255)" }
    "convert quads to rgba" in { toRgb("10101010") mustEqual "rgba(16,16,16,16)" }
    "convert half triples to rgb" in { toRgb("fff") mustEqual "rgb(255,255,255)" }
    "convert half quads to rgba" in { toRgb("ffff") mustEqual "rgba(255,255,255,255)" }
    "retain other hash values" in { toRgb("12") mustEqual "#12" }
  }

  "Generating CSS output" should {
    "produce RGB values in place of named colours" in { output(NamedColor("red")) mustEqual "rgb(255,0,0)"}
    "produce RGB values in place of hash colours" in { output(HashColor("ff0000")) mustEqual "rgb(255,0,0)"}
    "produce RGB values from RGB colours" in { output(RgbColor("255","0","0")) mustEqual "rgb(255,0,0)"}
  }

}
