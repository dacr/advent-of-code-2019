package adventofcode

import better.files._

import scala.math._

object Day2 {

  object Part1 {

    def executeProgram(program: Array[Int], pos:Int=0): Array[Int] = {
      program(pos) match {
        case 1 =>
          program(program(pos+3)) = program(program(pos+1))+program(program(pos+2))
          executeProgram(program, pos+4)
        case 2 =>
          program(program(pos+3)) = program(program(pos+1))*program(program(pos+2))
          executeProgram(program, pos+4)
        case 99 => program
      }
    }

    def executeInputFile(): Int = {
      val inputFile = "data" / "day2" / "part1" / "input.txt"
      val program = inputFile.contentAsString.split(",").map(_.toInt)
      program(1)=12
      program(2)=2
      val result = executeProgram(program)
      result(0)
    }
  }

  object Part2 {
    def executeProgram(program: Array[Int], pos:Int=0): Array[Int] = {
      program(pos) match {
        case 1 =>
          program(program(pos+3)) = program(program(pos+1))+program(program(pos+2))
          executeProgram(program, pos+4)
        case 2 =>
          program(program(pos+3)) = program(program(pos+1))*program(program(pos+2))
          executeProgram(program, pos+4)
        case 99 => program
      }
    }

    def execute(program:Array[Int],noun:Int, verb:Int): Int = {
      program(1)=noun
      program(2)=verb
      val result = executeProgram(program.clone())
      result(0)
    }

    def executeInputFile(goal:Int):Int = {
      val inputFile = "data" / "day2" / "part1" / "input.txt"
      val program = inputFile.contentAsString.split(",").map(_.toInt)
      var noun=0
      var verb=0
      val limit = program.size-1
      while(execute(program, noun, verb) != goal) {
        noun+=1
        if(noun > limit) {
          noun=0
          verb+=1
        }
      }
      100*noun + verb
    }

  }

}
