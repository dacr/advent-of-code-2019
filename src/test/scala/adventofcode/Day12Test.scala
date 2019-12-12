package adventofcode

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  import Day12.Part1._

  "input parser" should "" in {
    parse("<x=6, y=10, z=42>") shouldBe List(
      Moon(0, Vect(6,10,42),Vect(0,0,0))
    )
    parse("<x=6, y=10, z=42>\n<x=42, y=24, z=4>") shouldBe List(
      Moon(0,Vect(6,10,42),Vect(0,0,0)),
      Moon(1, Vect(42,24,4),Vect(0,0,0)),
      )
  }
}
