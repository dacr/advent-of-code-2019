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
            Day1.Part1.computeSumOfFuelRequirements(Iterator(12, 14)) shouldBe 4
            Day1.Part1.computeSumOfFuelRequirements(Iterator(14, 1969)) shouldBe 656
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
    "Day2" should {
      "Part1" should {
        "Computer" should {
          "be able to execute simple program" in {
            Day2.Part1.executeProgram(Array(1,0,0,0,99)) shouldBe Array(2,0,0,0,99)
            Day2.Part1.executeProgram(Array(2,3,0,3,99)) shouldBe Array(2,3,0,6,99)
            Day2.Part1.executeProgram(Array(2,4,4,5,99,0)) shouldBe Array(2,4,4,5,99,9801)
            Day2.Part1.executeProgram(Array(1,1,1,4,99,5,6,0,99)) shouldBe Array(30,1,1,4,2,5,6,0,99)
          }
          "be able to run with the provided input file" in {
            Day2.Part1.executeInputFile() shouldBe 5482655
          }
        }
      }
      "Part2" should {
        "Computer" should {
          "find the right noun and verb" in {
            Day2.Part2.executeInputFile(19690720) shouldBe 4967
          }
        }
      }
    }
  }
}
