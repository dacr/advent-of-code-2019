package adventofcode

import org.scalatest.{Matchers, WordSpec}

class AdventOfCodeTest extends WordSpec with Matchers {
  "Advent of code" must {
    "Day1" should {
      "Part1" should {
        "the fuel function" should {
          "gives the right result" in {
            Day1.Part1.computeFuel(12) shouldBe 2
            Day1.Part1.computeFuel(14) shouldBe 2
            Day1.Part1.computeFuel(1969) shouldBe 654
            Day1.Part1.computeFuel(100756) shouldBe 33583
          }
        }
        "The all modules fuel usage function" should {
          "gives the right sum" in {
            Day1.Part1.computeSumOfFuelRequirements(Iterator(12,14)) shouldBe 4
            Day1.Part1.computeSumOfFuelRequirements(Iterator(14,1969)) shouldBe 656
          }
        }
        "With the given input I" should {
          "find the following result" in {
            Day1.Part1.computeForInput() shouldBe 3263320
          }
        }
      }
      "Part2" should {
        "the total fuel consumption" should {
          "gives the right result" in {
            Day1.Part2.computeFuelFor(14d) shouldBe 2d
            Day1.Part2.computeFuelFor(12d) shouldBe 2d
            Day1.Part2.computeFuelFor(1969d) shouldBe 966d
            Day1.Part2.computeFuelFor(100756d) shouldBe 50346
          }
        }
        "With the given input I" should {
          "find the following result" in {
            Day1.Part2.computeForInput() should not be 4892086d
            Day1.Part2.computeForInput() should not be 4892293d
            Day1.Part2.computeForInput() shouldBe 4892135d
          }
        }
      }
    }
  }

}
