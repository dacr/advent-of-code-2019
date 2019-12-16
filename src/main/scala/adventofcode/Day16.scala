package adventofcode

import better.files._

import scala.annotation.tailrec

object Day16 {
  def fileToString(inputFile: File = "data" / "day16" / "input.txt"): String = inputFile.contentAsString

  val basePatternDefault = Array(0, 1, 0, -1)
  val basePatternDefaultSize = basePatternDefault.length

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

    @inline def lastDigit(value: Int): Int = math.abs(value % 10)


    @inline def patternGenerator(goalPosition: Int, index: Int): Int = {
      val i = index / goalPosition
      basePatternDefault(i % basePatternDefaultSize)
    }

    def process(input: String, iterations: Int = 100): String = {
      val result = processDigits((input * 10000).map(_.toInt - 48).toArray, iterations)
      val toSkip = input.take(7).mkString.toInt
      result.drop(toSkip).take(8).mkString
    }

    def processDigits(digits: Array[Int], iterations: Int): Array[Int] = {
      val digitsCount = digits.length
      println(s"### $digitsCount for $iterations iterations ###")
      //var sum = 0
      var timestamp = System.currentTimeMillis()
      val started = timestamp
      @tailrec
      def worker(digits1: Array[Int], digits2: Array[Int], currentIteration: Int): Array[Int] = {
        if (currentIteration == iterations) digits1
        else {
          var goalPosition = 0
          while (goalPosition < digitsCount) {
            if (goalPosition%1000 == 0) {
              val duration = System.currentTimeMillis()-timestamp
              timestamp = System.currentTimeMillis()
              val estim = duration * (iterations*digitsCount+goalPosition) / 1000 / 3600
              println(s"--- $currentIteration/$iterations - $goalPosition/$digitsCount - ${duration}ms - ${estim} hours ---")
            }
            var sum = 0
            var index = goalPosition
            while (index < digitsCount) {
              val i = (index+1) / (goalPosition+1)
              val r = (index+1) % (goalPosition+1)
              val factor = basePatternDefault(i % basePatternDefaultSize)
              if (factor == 0 && r == 0) index += goalPosition+1
              else {
                sum += digits1(index) * factor
                index += 1
              }
            }
            digits2(goalPosition) = lastDigit(sum)
            goalPosition += 1
          }
          worker(digits2, digits1, currentIteration + 1)
        }
      }

      worker(digits, digits.clone(), 0)
    }
  }

  def main(args: Array[String]): Unit = {
    import Part2._
    val result = process(fileToString())
    println(result)
  }
}

