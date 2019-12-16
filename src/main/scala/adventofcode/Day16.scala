package adventofcode

import better.files._

object Day16 {
  def fileToString(inputFile: File = "data" / "day16" / "input.txt"): String = inputFile.contentAsString

  val basePatternDefault = Array(0, 1, 0, -1)

  object Part1 {
    def process(input: String, iterations: Int = 100): String = {
      processDigits(input.map(_.toInt - 48).toList, iterations).take(8).mkString("")
    }

    def makePattern(position: Int, length: Int, basePattern: Array[Int] = basePatternDefault): List[Int] = {
      def worker(currentPosition: Int, pattern: List[Int]): List[Int] = {
        if (currentPosition * position >= length) pattern.reverse.take(length)
        else {
          val patternValue = basePattern(currentPosition % basePattern.length)
          val newPattern = (1 to position).foldLeft(pattern) { case (pat, _) => patternValue :: pat }
          worker(currentPosition + 1, newPattern)
        }
      }

      worker(0, List.empty)
    }


    def lastDigit(value: Int): Int = math.abs(value % 10)

    def processDigits(digits: List[Int], iterations: Int): List[Int] = {
      val patterns: List[List[Int]] =
        (1 to digits.size)
          .map { position => makePattern(position, digits.length + 1) }
          .toList

      def worker(currentDigits: List[Int], currentIteration: Int): List[Int] = {
        println(s"--- iteration $currentIteration ---")
        if (currentIteration == iterations) currentDigits
        else {
          val newDigits = patterns.map { pattern => lastDigit(currentDigits.zip(pattern.tail).map { case (a, b) => a * b }.sum) }
          worker(newDigits, currentIteration + 1)
        }
      }

      worker(digits, 0)
    }

  }
  object Part2 {

    def main(args: Array[String]): Unit = {
      //val result = process(fileToString() * 10000)
      //println(result)
    }
  }
}

