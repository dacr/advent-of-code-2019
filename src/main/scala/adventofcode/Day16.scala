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


    @inline def patternGenerator(goalPosition:Int, index:Int):Int = {
      val i = index / goalPosition
      basePatternDefault(i % basePatternDefaultSize)
    }

    def process(input: String, iterations: Int = 100): String = {
      processDigits(input.map(_.toInt - 48).toArray, iterations).take(8).mkString("")
    }

    def processDigits(digits: Array[Int], iterations: Int): Array[Int] = {
      val digitsCount = digits.length
      var sum=0
      @tailrec
      def worker(digits1: Array[Int], digits2:Array[Int], currentIteration: Int): Array[Int] = {
        println(s"--- $currentIteration ---")
        if (currentIteration == iterations) digits1
        else {
          var goalPosition=1
          while( goalPosition <= digitsCount ) {
            sum = 0
            var index = 0
            while (index < digitsCount) {
              sum += digits1(index) * patternGenerator(goalPosition, index + 1)
              index+=1
            }
            digits2(goalPosition-1) = lastDigit(sum)
            goalPosition+=1
          }
          worker(digits2, digits1, currentIteration + 1)
        }
      }

      worker(digits, digits.clone(), 0)
    }


    }
  def main(args: Array[String]): Unit = {
    import Part2._
    val result = process(fileToString() * 10000)
    println(result)
  }
}

