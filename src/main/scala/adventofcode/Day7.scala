package adventofcode

import better.files._


object Day7 {

  object Part1 {

    def executeProgram(program: Vector[Int], inputs:List[Int], pos:Int, outputs:List[Int]=Nil): (Vector[Int],List[Int]) = {
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
          executeProgram(newProgram, inputs, pos+4, outputs)
        case 2 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          val newProgram = writeValue(program, pos+3, thirdParamMode, firstParam * secondParam)
          executeProgram(newProgram, inputs, pos+4, outputs)
        case 3 =>
          val address = program(pos+1)
          val extendedProgram = program
          val newProgram = extendedProgram.updated(address, inputs.head)
          executeProgram(newProgram, inputs.tail, pos+2, outputs)
        case 4 =>
          val outputValue = readValue(program, pos+1, firstParamMode)
          executeProgram(program, inputs, pos+2, outputValue::outputs)
        case 5 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          if (firstParam != 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos+3, outputs)
        case 6 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          if (firstParam == 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos+3, outputs)
        case 7 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          val newProgram = writeValue(program, pos+3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos+4, outputs)
        case 8 =>
          val firstParam = readValue(program, pos+1, firstParamMode)
          val secondParam = readValue(program, pos+2, secondParamMode)
          val newProgram = writeValue(program, pos+3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos+4, outputs)
        case 99 => (program, outputs)
      }
    }

    def execute(program:Vector[Int], inputs:List[Int]):(Vector[Int],List[Int]) = executeProgram(program, inputs, 0)
    def execute(program: String, inputs:List[Int]): (Vector[Int],List[Int]) = execute(program.split(",").toVector.map(_.toInt), inputs)


    def executeAmplify(program: Vector[Int], configurations: Iterable[Int], input: Int):Int = {
      def run(program:Vector[Int], remainingConfigurations:Iterable[Int], input:Int):Int = {
        remainingConfigurations.headOption match {
          case None => input // == output of the previous amplifier and so the final result
          case Some(configuration) =>
            val inputs = configuration::input::Nil
            val output = execute(program, inputs) match {case (_, results)=> results.head}
            run(program, remainingConfigurations.tail, output)
        }
      }
      run(program, configurations, input)
    }

    def amplify(program:Vector[Int], input:Int):Int = {
      val results:Iterator[Int] = for {
        permutation <- 0.to(4).permutations
      } yield {
        executeAmplify(program, permutation, input)
      }
      results.max
    }

    def amplify(program: String, input:Int):Int = amplify(program.split(",").toVector.map(_.toInt), input)

    def amplifyInputFile(): Int = {
      val inputFile = "data" / "day7" / "part1" / "input.txt"
      amplify(inputFile.contentAsString, 0)
    }

  }


  // ========================================================================================

  object Part2 {

  }
}
