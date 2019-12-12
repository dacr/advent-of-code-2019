package adventofcode

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  import Day12.Part1._

  "simulation" should "return the right result" in {
    simulate(parse(fileToString())) should not be 156410 //too high
    simulate(parse(fileToString())) should not be 395 //too low
    simulate(parse(fileToString())) shouldBe 13045
  }
  it should "be able to return to a previous state" in {
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe > (126296) // Too low
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe > (167127) // Too low
    howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())) shouldBe 42
  }
}
