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
            Day2.Part1.executeProgram(Array(1, 0, 0, 0, 99)) shouldBe Array(2, 0, 0, 0, 99)
            Day2.Part1.executeProgram(Array(2, 3, 0, 3, 99)) shouldBe Array(2, 3, 0, 6, 99)
            Day2.Part1.executeProgram(Array(2, 4, 4, 5, 99, 0)) shouldBe Array(2, 4, 4, 5, 99, 9801)
            Day2.Part1.executeProgram(Array(1, 1, 1, 4, 99, 5, 6, 0, 99)) shouldBe Array(30, 1, 1, 4, 2, 5, 6, 0, 99)
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
    "Day3" should {
      "Part1" should {
        "work with basic examples" in {
          Day3.Part1.closestIntersection(
            """R75,D30,R83,U83,L12,D49,R71,U7,L72
              |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin) shouldBe 159
          Day3.Part1.closestIntersection(
            """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
              |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin) shouldBe 135
        }
        "compute the right result with the given input file" in {
          Day3.Part1.computeForInputFile() shouldBe 1225
        }
      }
      "Part2" should {
        "be able to compute the best intersection with basics examples" in {
          Day3.Part2.bestIntersection(
            """R75,D30,R83,U83,L12,D49,R71,U7,L72
              |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin) shouldBe 610
          Day3.Part2.bestIntersection(
            """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
              |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin) shouldBe 410
        }
        "compute the best interestion with the given input file" in {
          Day3.Part2.computeForInputFile() shouldBe 107036
        }
      }
    }
    "Day4" should {
      "Part1" should {
        "provides helper functions" should {
          "passwordFromStringToIntArray" in {
            Day4.Part1.passwordFromStringToIntArray("012345") shouldBe Array(0,1,2,3,4,5)
          }
          "implement a password checker" in {
            Day4.Part1.checkStringValidity("111111") shouldBe true
            Day4.Part1.checkStringValidity("123349") shouldBe true
            Day4.Part1.checkStringValidity("001389") shouldBe true
            Day4.Part1.checkStringValidity("223450") shouldBe false
            Day4.Part1.checkStringValidity("123789") shouldBe false
          }
          "return valid password count between a given range" in {
            Day4.Part1.validPasswordCount("153517","630395") should not be 63551 // Too high value
            Day4.Part1.validPasswordCount("153517","630395") shouldBe 1729
          }
        }
      }
      "Part2" should {
        "provides an additional constraint on the number of repeated digits" in  {
          Day4.Part2.checkStringValidity("111111") shouldBe false
          Day4.Part2.checkStringValidity("111122") shouldBe true
          Day4.Part2.checkStringValidity("123349") shouldBe true
          Day4.Part2.checkStringValidity("001389") shouldBe true
          Day4.Part2.checkStringValidity("223450") shouldBe false
          Day4.Part2.checkStringValidity("123789") shouldBe false
        }
        "return valid password count between a given range with the new constraint" in {
          Day4.Part2.validPasswordCount("153517","630395") shouldBe 1172
        }
      }
    }
    "Day5" should {
      "Part1" should {
        "engine compute simple programs" in {
          Day5.Part1.execute("1002,4,3,4,33", 1)._1 shouldBe Vector(1002,4,3,4,99)
          Day5.Part1.execute("11102,1,2,0,99", 1)._1 shouldBe Vector(11102,1,2,2,99)
          Day5.Part1.execute("1101,100,-1,4,0", 1)._1 shouldBe Vector(1101,100,-1,4,99)
        }
        "engine execute provided program with 1 as input" in {
          val (finalState, results) = Day5.Part1.executeInputFile()
          results.head shouldBe 9219874
        }
      }
      "Part2" should {
        "engine execute simple programs with the right result" in {
          val program =
            """3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
              |1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
              |999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99""".stripMargin.replaceAll("\n", "")
          Day5.Part2.execute(program, 5)._2.head shouldBe 999
          Day5.Part2.execute(program, 8)._2.head shouldBe 1000
          Day5.Part2.execute(program, 42)._2.head shouldBe 1001
        }
        "engine execute provided program with 1 as input" in {
          val (finalState, results) = Day5.Part2.executeInputFile()
          results.head shouldBe 5893654
        }
      }
    }
    "Day6" should {
      "Part1" should {
        "be able to build and compute depths sum" in {
          val treeDesc =
            """COM)B
              |B)C
              |C)D
              |D)E
              |E)F
              |B)G
              |G)H
              |D)I
              |E)J
              |J)K
              |K)L""".stripMargin
          Day6.Part1.executeWithInputString(treeDesc) shouldBe 42
        }
        "be able to find the result of the provided file" in {
          Day6.Part1.executeWithInputFile() shouldBe 234446
        }
      }
      "Part2" should {
        "implement a tree distance algorithm" in {
          val treeDesc =
            """COM)B
              |B)C
              |C)D
              |D)E
              |E)F
              |B)G
              |G)H
              |D)I
              |E)J
              |J)K
              |K)L
              |K)YOU
              |I)SAN""".stripMargin
          Day6.Part2.executeWithInputString(treeDesc) shouldBe 4
        }
        "give the right result with the input file" in {
          Day6.Part2.executeWithInputFile() shouldBe 385
        }
      }
    }
  }
}
