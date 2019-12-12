package adventofcode

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  import Day12.Part1._

  "parser" should "parse input instructions" in {
    parse("<x=6, y=10, z=42>") shouldBe List(
      Moon(3, Vect(6,10,42),Vect(0,0,0))
    )
    parse("<x=6, y=10, z=42>\n<x=42, y=24, z=4>") shouldBe List(
      Moon(3,Vect(6,10,42),Vect(0,0,0)),
      Moon(5, Vect(42,24,4),Vect(0,0,0)),
      )
  }
  "simulation" should "return the right result" in {
    simulate(parse(fileToString())) should not be 78078410000L //too high
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
