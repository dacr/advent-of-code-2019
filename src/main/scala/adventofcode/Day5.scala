package adventofcode

import better.files._
import scala.math._


object Day5 {

  object Part1 {

    def executeProgram(program: Vector[Int], input:Int, pos:Int, outputs:List[Int]=Nil): (Vector[Int],List[Int]) = {
      val instruction = program(pos).toString
      val opcode = instruction.reverse.take(2).reverse.toInt
      val (firstParamMode,secondParamMode,thirdParamMode) = instruction.reverse.drop(2).toVector.map(_.toInt - 48) match {
        case Vector() => (0,0,0)
        case Vector(a) => (a,0,0)
        case Vector(a,b) => (a,b,0)
        case Vector(a,b,c) => (c,b,a)
      }
      def readValue(program:Vector[Int], paramPos:Int, paramMode:Int) = {
        paramMode match {
          case 0 => program(program(paramPos)) // address mode
          case 1 => program(paramPos) // immediate mode
        }
      }
      def writeValue(program:Vector[Int], paramPos:Int, paramMode:Int, value:Int): Vector[Int] = {
        paramMode match {
          case 0 => program.updated(program(paramPos), value) // address mode
          case 1 => program.updated(paramPos, value) // immediate mode
        }
      }
      opcode match {
        case 1 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          val newProgram = writeValue(program, pos+3, thirdParamMode, firstParam + secondParam)
          executeProgram(newProgram, input, pos+4, outputs)
        case 2 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          val newProgram = writeValue(program, pos+3, thirdParamMode, firstParam * secondParam)
          executeProgram(newProgram, input, pos+4, outputs)
        case 3 =>
          val address = program(pos+1)
          println(s"input=$input")
          //val extendedProgram = if (address > program.size - 1) program.appendedAll(Vector.fill(address-program.size+1)(0)) else program
          val extendedProgram = program
          val newProgram = extendedProgram.updated(address, input)
          executeProgram(newProgram, input, pos+2, outputs)
        case 4 =>
          val outputValue = program(program(pos+1))
          executeProgram(program, input, pos+2, outputValue::outputs)
        case 99 => (program, outputs)
      }
    }

    def execute(program:Vector[Int], input:Int):(Vector[Int],List[Int]) = {
      executeProgram(program, input, 0)
    }

    def execute(program: String, input:Int): (Vector[Int],List[Int]) = {
      execute(program.split(",").toVector.map(_.toInt), input)
    }


    def executeInputFile(): (Vector[Int],List[Int]) = {
      val inputFile = "data" / "day5" / "part1" / "input.txt"
      execute(inputFile.contentAsString, 1)
    }
  }

  // ========================================================================================

  object Part2 {
  }

}
