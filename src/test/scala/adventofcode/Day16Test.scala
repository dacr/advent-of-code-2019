package adventofcode

import org.scalatest.{Matchers, WordSpec}

class Day16Test extends WordSpec with Matchers {

  "Day16" should {
    import Day16._

    "Part1" should {
      import Day16.Part1._
      "makePattern" should {
        "give the right value for given samples" in {
          makePattern(1, 4) shouldBe List(0, 1, 0, -1)
          makePattern(1, 8) shouldBe List(0, 1, 0, -1, 0, 1, 0, -1)
          makePattern(2, 5) shouldBe List(0, 0, 1, 1, 0)
        }
      }

      "reference case tests" in {
        process("12345678", 4) shouldBe "01029498"
        process("80871224585914546619083218645595") shouldBe "24176176"
        process("19617804207202209144916044189917") shouldBe "73745418"
        process("69317163492948606335995924319873") shouldBe "52432133"
      }
      "give the right result with the provided file" in {
        process(fileToString()) shouldBe "42205986"
      }
    }

    "Part2" should {
      import Day16.Part2._
      "pattern generator" should {
        "give the right value for given samples" in {
          0.until(4).map(n => patternGenerator(1, n)) shouldBe List(0, 1, 0, -1)
          0.until(8).map(n => patternGenerator(1, n)) shouldBe List(0, 1, 0, -1, 0, 1, 0, -1)
          0.until(4).map(n => patternGenerator(2, n)) shouldBe List(0, 0, 1, 1)
        }
      }

      "should run quickly on previous examples" in {
        process("12345678", 4) shouldBe "01029498"
        process("80871224585914546619083218645595") shouldBe "24176176"
        process("19617804207202209144916044189917") shouldBe "73745418"
        process("69317163492948606335995924319873") shouldBe "52432133"
      }
      "give the right result with the provided file" in {
        process(fileToString()) shouldBe "42205986"
      }

      "give the right result with the provided file * 10000" in {
        process(fileToString()) shouldBe "42205986"
      }

    }
  }
}