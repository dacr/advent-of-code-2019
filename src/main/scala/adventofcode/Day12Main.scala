package adventofcode

import Day12.Part1._

object Day12Main {
  def main(args: Array[String]): Unit = {
    println(howManyStepsToGoBackToAnAlreadySeenState(parse(fileToString())))
  }
}
