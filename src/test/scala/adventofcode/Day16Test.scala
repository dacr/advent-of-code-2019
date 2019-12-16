package adventofcode

import org.scalatest.{Matchers, WordSpec}

class Day16Test extends WordSpec with Matchers {

  "Day16" should {
    import Day16._

    "makePattern" should {
      "give the same value for pos 0" in {
        makePattern(1, 4) shouldBe List(0,1,0,-1)
        makePattern(1, 8) shouldBe List(0,1,0,-1,0,1,0,-1)
        makePattern(2, 5) shouldBe List(0,0,1,1,0)
      }
    }

    "Part1" should {
      "reference case tests" in {
        process("12345678",4) shouldBe "01029498"
        process("80871224585914546619083218645595") shouldBe "24176176"
        process("19617804207202209144916044189917") shouldBe "73745418"
        process("69317163492948606335995924319873") shouldBe "52432133"
      }
      "give the right result with the provided file" in {
        process(fileToString()) shouldBe "42205986"
      }
    }
  }

}
