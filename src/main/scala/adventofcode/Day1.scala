package adventofcode

import scala.math._
import better.files.Dsl._
import better.files._

import scala.annotation.tailrec

object Day1 {

  object Part1 {
    def computeFuel(mass:Double):Double = {
      floor(mass / 3) - 2
    }
    def computeSumOfFuelRequirements(masses:Iterator[Double]):Double = {
      masses.map(computeFuel).sum
    }
    def computeForInput(): Double = {
      val inputFile = "data" / "day1" / "part1" / "input.txt"
      computeSumOfFuelRequirements(inputFile.lineIterator.map(_.toDouble))
    }
  }

  object Part2 {
    def computeFuelFor(mass: Double):Double = {
      val fuel = floor(mass / 3) - 2
      if (fuel <= 0) 0d
      else fuel + computeFuelFor(fuel)
    }
    def computeForInput(): Double = {
      val inputFile = "data" / "day1" / "part1" / "input.txt"
      inputFile.lineIterator.map(_.toDouble).map(computeFuelFor).sum
    }
  }

}
