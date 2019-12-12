package adventofcode

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {

  "simulation" should "return the right result" in {
    import Day12.Part1._
    simulate(parse(fileToString())) should not be 156410 //too high
    simulate(parse(fileToString())) should not be 395 //too low
    simulate(parse(fileToString())) shouldBe 13045
  }

  it should "return the expected result" in {
    import Day12.Part2._
    val input =
      """<x=-1, y=0, z=2>
        |<x=2, y=-10, z=-7>
        |<x=4, y=-8, z=8>
        |<x=3, y=5, z=-1>""".stripMargin
    howManyStepsToGoBackToAnAlreadySeenState(parse(input), limit = 5000L) shouldBe 2772L
  }

  it should "return the right result quickly" in {
    import Day12.Part2._
    val input =
      """<x=-8, y=-10, z=0>
        |<x=5, y=5, z=10>
        |<x=2, y=-7, z=3>
        |<x=9, y=-8, z=-3>""".stripMargin
    howManyStepsToGoBackToAnAlreadySeenState(parse(input)) shouldBe 4686774924L
  }

  it should "be able to return to a previous state" ignore {
    import Day12.Part2._
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe > (126296) // Too low
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe > (167127) // Too low
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe 42
  }
}
