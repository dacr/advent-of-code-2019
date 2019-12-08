package adventofcode

import better.files._

import scala.annotation.tailrec
import akka.actor.typed.ActorSystem
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors


object Day7 {

  object Part1 {

    def executeProgram(program: Vector[Int], inputs: List[Int], pos: Int, outputs: List[Int] = Nil): (Vector[Int], List[Int]) = {
      val instruction = program(pos).toString
      val opcode = instruction.reverse.take(2).reverse.toInt
      val (firstParamMode, secondParamMode, thirdParamMode) = instruction.reverse.drop(2).toVector.map(_.toInt - 48) match {
        case Vector() => (0, 0, 0)
        case Vector(a) => (a, 0, 0)
        case Vector(a, b) => (a, b, 0)
        case Vector(a, b, c) => (c, b, a)
      }

      def readValue(program: Vector[Int], paramPos: Int, paramMode: Int) = {
        paramMode match {
          case 0 => program(program(paramPos)) // address mode
          case 1 => program(paramPos) // immediate mode
        }
      }

      def writeValue(program: Vector[Int], paramPos: Int, paramMode: Int, value: Int): Vector[Int] = {
        paramMode match {
          case 0 => program.updated(program(paramPos), value) // address mode
          case 1 => program.updated(paramPos, value) // immediate mode
        }
      }

      opcode match {
        case 1 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, firstParam + secondParam)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 2 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, firstParam * secondParam)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 3 =>
          val address = program(pos + 1)
          val extendedProgram = program
          val newProgram = extendedProgram.updated(address, inputs.head)
          executeProgram(newProgram, inputs.tail, pos + 2, outputs)
        case 4 =>
          val outputValue = readValue(program, pos + 1, firstParamMode)
          executeProgram(program, inputs, pos + 2, outputValue :: outputs)
        case 5 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          if (firstParam != 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos + 3, outputs)
        case 6 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          if (firstParam == 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos + 3, outputs)
        case 7 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 8 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 99 => (program, outputs)
      }
    }

    def execute(program: Vector[Int], inputs: List[Int]): (Vector[Int], List[Int]) = executeProgram(program, inputs, 0)

    def execute(program: String, inputs: List[Int]): (Vector[Int], List[Int]) = execute(program.split(",").toVector.map(_.toInt), inputs)


    def executeAmplify(program: Vector[Int], configurations: Iterable[Int], input: Int): Int = {
      def run(program: Vector[Int], remainingConfigurations: Iterable[Int], input: Int): Int = {
        remainingConfigurations.headOption match {
          case None => input // == output of the previous amplifier and so the final result
          case Some(configuration) =>
            val inputs = configuration :: input :: Nil
            val output = execute(program, inputs) match {
              case (_, results) => results.head
            }
            run(program, remainingConfigurations.tail, output)
        }
      }

      run(program, configurations, input)
    }

    def amplify(program: Vector[Int], input: Int): Int = {
      val results: Iterator[Int] = for {
        permutation <- 0.to(4).permutations
      } yield {
        executeAmplify(program, permutation, input)
      }
      results.max
    }

    def amplify(program: String, input: Int): Int = amplify(program.split(",").toVector.map(_.toInt), input)

    def amplifyInputFile(): Int = {
      val inputFile = "data" / "day7" / "part1" / "input.txt"
      amplify(inputFile.contentAsString, 0)
    }

  }


  // ========================================================================================

/*
  object Part2 {

    object ProgramActor {
      sealed trait ProgramMessage

      case class Setup(linkedTo:ActorRef[Input]) extends ProgramMessage
      case class Input(value:Int) extends ProgramMessage

      def init(code: Vector[Int]):Behavior[ProgramMessage] = {
        Behaviors.receiveMessage { setup: Setup =>
          val replyTo = setup.linkedTo
          run(code, replyTo)
        }
      }

      def run(code: Vector[Int], replyTo:ActorRef[Input], pointer:Int=0, inputs:Vector[Int]=Vector.empty): Behavior[ProgramMessage] =
          Behaviors.receiveMessage { message:Input =>
            process(code, replyTo, pointer, inputs :+ message.value)
        }

      def process(code: Vector[Int], replyTo:ActorRef[Input], pointer:Int, inputs:Vector[Int]): Behavior[ProgramMessage] = {
        val instruction = code(pointer).toString
        val opcode = instruction.reverse.take(2).reverse.toInt
        val (firstParamMode, secondParamMode, thirdParamMode) = instruction.reverse.drop(2).toVector.map(_.toInt - 48) match {
          case Vector() => (0, 0, 0)
          case Vector(a) => (a, 0, 0)
          case Vector(a, b) => (a, b, 0)
          case Vector(a, b, c) => (c, b, a)
        }
        opcode match {
          case 1 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, firstParam + secondParam)
            process(newProgram, replyTo, pointer + 4, inputs)
          case 2 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, firstParam * secondParam)
            process(newProgram, replyTo, pointer + 4, inputs)
          case 3 if inputs.isEmpty =>
            ProgramActor.run(code, replyTo, pointer, inputs) // request for more inputs
          case 3 =>
            val address = code(pointer + 1)
            val extendedProgram = code
            val newCode = extendedProgram.updated(address, inputs.head)
            process(newCode, replyTo, pointer + 2, inputs.tail)
          case 4 =>
            val outputValue = readValue(code, pointer + 1, firstParamMode)
            replyTo ! Input(outputValue)
            process(code, replyTo, pointer + 2, inputs)
          case 5 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam != 0)
              process(code, replyTo, secondParam, inputs)
            else
              process(code, replyTo, pointer + 3, inputs)
          case 6 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam == 0)
              process(code, replyTo, secondParam, inputs)
            else
              process(code, replyTo, pointer + 3, inputs)
          case 7 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
            process(newProgram, replyTo, pointer + 4, inputs)
          case 8 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
            process(newProgram, replyTo, pointer + 4, inputs)
          case 99 =>
            Behaviors.stopped
        }
      }
    }


    object EngineActor {

      sealed trait Control

      case class Start(value:Int) extends Control
      case class Result(value:Int) extends Control


      def apply():Behavior[Control] = Behaviors.setup{ context =>
        Behaviors.receiveMessage {
          case message: Start =>

            Behaviors.receiveMessage{
              case Result(value) =>
                Behaviors.stopped
            }
        }
      }
    }


    val system: ActorSystem[EngineActor.Control] = ActorSystem(EngineActor(), "solver")
    implicit val ec = system.executionContext
    system ! EngineActor.Start(0)
    system.getWhenTerminated







    def readValue(program: Vector[Int], paramPos: Int, paramMode: Int): Int = {
      paramMode match {
        case 0 => program(program(paramPos)) // address mode
        case 1 => program(paramPos) // immediate mode
      }
    }

    def writeValue(program: Vector[Int], paramPos: Int, paramMode: Int, value: Int): Vector[Int] = {
      paramMode match {
        case 0 => program.updated(program(paramPos), value) // address mode
        case 1 => program.updated(paramPos, value) // immediate mode
      }
    }

    def executeProgram(program: Vector[Int], inputs: List[Int], pos: Int, outputs: List[Int] = Nil): (Vector[Int], List[Int]) = {
      val instruction = program(pos).toString
      val opcode = instruction.reverse.take(2).reverse.toInt
      val (firstParamMode, secondParamMode, thirdParamMode) = instruction.reverse.drop(2).toVector.map(_.toInt - 48) match {
        case Vector() => (0, 0, 0)
        case Vector(a) => (a, 0, 0)
        case Vector(a, b) => (a, b, 0)
        case Vector(a, b, c) => (c, b, a)
      }

      opcode match {
        case 1 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, firstParam + secondParam)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 2 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, firstParam * secondParam)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 3 =>
          val address = program(pos + 1)
          val extendedProgram = program
          val newProgram = extendedProgram.updated(address, inputs.head)
          executeProgram(newProgram, inputs.tail, pos + 2, outputs)
        case 4 =>
          val outputValue = readValue(program, pos + 1, firstParamMode)
          executeProgram(program, inputs, pos + 2, outputValue :: outputs)
        case 5 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          if (firstParam != 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos + 3, outputs)
        case 6 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          if (firstParam == 0)
            executeProgram(program, inputs, secondParam, outputs)
          else
            executeProgram(program, inputs, pos + 3, outputs)
        case 7 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 8 =>
          val firstParam = readValue(program, pos + 1, firstParamMode)
          val secondParam = readValue(program, pos + 2, secondParamMode)
          val newProgram = writeValue(program, pos + 3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
          executeProgram(newProgram, inputs, pos + 4, outputs)
        case 99 => (program, outputs)
      }
    }

    def execute(program: Vector[Int], inputs: List[Int]): (Vector[Int], List[Int]) = executeProgram(program, inputs, 0)

    def execute(program: String, inputs: List[Int]): (Vector[Int], List[Int]) = execute(program.split(",").toVector.map(_.toInt), inputs)


    def executeAmplify(program: Vector[Int], configurations: Iterable[Int], input: Int): Int = {
      @tailrec
      def run(program: Vector[Int], remainingConfigurations: Iterable[Int], input: Int): Int = {
        remainingConfigurations.headOption match {
          case None => input // == output of the previous amplifier and so the final result
          case Some(configuration) =>
            val inputs = configuration :: input :: Nil
            val output = execute(program, inputs) match {
              case (_, results) => results.head
            }
            run(program, remainingConfigurations.tail, output)
        }
      }

      run(program, configurations, input)
    }

    def amplify(program: Vector[Int], input: Int): Int = {
      val results: Iterator[Int] = for {
        permutation <- 5.to(9).permutations
      } yield {
        println(permutation)
        executeAmplify(program, permutation, input)
      }
      results.max
    }

    def amplify(program: String, input: Int): Int = amplify(program.split(",").toVector.map(_.toInt), input)

    def amplifyInputFile(): Int = {
      val inputFile = "data" / "day7" / "part1" / "input.txt"
      amplify(inputFile.contentAsString, 0)
    }
  }
*/

}
