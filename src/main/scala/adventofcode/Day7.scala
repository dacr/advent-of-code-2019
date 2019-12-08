package adventofcode

import better.files._

import scala.annotation.tailrec
import akka.actor.typed.ActorSystem
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._



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

  object Part2 {


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


    object ProgramActor {
      sealed trait ProgramMessage

      case class Setup(linkedTo:ActorRef[Input], lastResponseTo:ActorRef[EngineActor.Control]) extends ProgramMessage
      case class Input(value:Int) extends ProgramMessage

      def apply(code: Vector[Int]):Behavior[ProgramMessage] = Behaviors.setup { context => {
          Behaviors.receiveMessage {
            case setup: Setup =>
              run(context, code, setup.linkedTo, setup.lastResponseTo)
          }
        }
      }

      def run(context:ActorContext[ProgramMessage], code: Vector[Int], replyTo:ActorRef[Input], lastResponseTo:ActorRef[EngineActor.Control], pointer:Int=0, inputs:Vector[Int]=Vector.empty, lastOutput:Option[Int]=None): Behavior[ProgramMessage] =
          Behaviors.receiveMessage { case message:Input =>
            process(context, code, replyTo, lastResponseTo, pointer, inputs :+ message.value, lastOutput)
        }

      def process(context:ActorContext[ProgramMessage], code: Vector[Int], replyTo:ActorRef[Input], lastResponseTo:ActorRef[EngineActor.Control], pointer:Int, inputs:Vector[Int], lastOutput:Option[Int]): Behavior[ProgramMessage] = {
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
            process(context, newProgram, replyTo, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 2 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, firstParam * secondParam)
            process(context, newProgram, replyTo, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 3 if inputs.isEmpty =>
            ProgramActor.run(context, code, replyTo, lastResponseTo, pointer, inputs, lastOutput) // request for more inputs
          case 3 =>
            val address = code(pointer + 1)
            val extendedProgram = code
            val newCode = extendedProgram.updated(address, inputs.head)
            process(context, newCode, replyTo, lastResponseTo, pointer + 2, inputs.tail, lastOutput)
          case 4 =>
            val outputValue = readValue(code, pointer + 1, firstParamMode)
            replyTo ! Input(outputValue)
            process(context, code, replyTo, lastResponseTo, pointer + 2, inputs, Some(outputValue))
          case 5 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam != 0)
              process(context, code, replyTo, lastResponseTo, secondParam, inputs, lastOutput)
            else
              process(context, code, replyTo, lastResponseTo, pointer + 3, inputs, lastOutput)
          case 6 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam == 0)
              process(context, code, replyTo, lastResponseTo, secondParam, inputs, lastOutput)
            else
              process(context, code, replyTo, lastResponseTo, pointer + 3, inputs, lastOutput)
          case 7 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
            process(context, newProgram, replyTo, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 8 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
            process(context, newProgram, replyTo, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 99 =>
            lastResponseTo ! EngineActor.Result(lastOutput, context.self)
            Behaviors.stopped
        }
      }
    }


    object EngineActor {

      sealed trait Control

      case class Start(code:Vector[Int], value:Int, configurations:Iterable[Int]) extends Control
      case class Result(value:Option[Int], from:ActorRef[ProgramActor.ProgramMessage]) extends Control


      def apply():Behavior[Control] = Behaviors.setup{ context =>
        Behaviors.receiveMessage {
          case Start(code, value, configurations) =>
            // Create processing actors
            val programActors = configurations.zipWithIndex.map{ case (config, position) =>
              val actorName = "system-"+configurations.mkString+"-"+position
              context.spawn(ProgramActor(code), actorName)
            }.toList
            // Send setup message
            programActors.zip(programActors.tail:+ programActors.head).foreach{ case (leftActor, rightActor) =>
              leftActor ! ProgramActor.Setup(rightActor, context.self)
            }
            // Send actor configuration input
            programActors.zip(configurations).foreach {case (programActorRef, config) =>
              programActorRef ! ProgramActor.Input(config)
            }
            // Send first actor "boot" input
            programActors.head ! ProgramActor.Input(value)

            // last programActor from which we are interested by final result
            val lastProgramActorRef = programActors.last

            Behaviors.receiveMessage{
              case Result(value, from) if from == lastProgramActorRef =>
                println(s"***$value*** for ${configurations.mkString("-")}")
                Behaviors.stopped
              case Result(value, from) => // we don't care about them
                //println(s"!!!!! ***$value*** for ${configurations.mkString("-")} from ${from.path}")
                Behaviors.same
            }
        }
      }
    }




    def executeAmplify(code: Vector[Int], configurations: Iterable[Int], input: Int): Int = {
      val system: ActorSystem[EngineActor.Control] = ActorSystem(EngineActor(), "solver")
      implicit val ec = system.executionContext
      system ! EngineActor.Start(code, 0, configurations)
      val future = system.whenTerminated
      Await.ready(future, 30.seconds)
      42
    }

    def amplify(code: Vector[Int], input: Int): Int = {
      val results: Iterator[Int] = for {
        permutation <- 5.to(9).permutations
      } yield {
        executeAmplify(code, permutation, input)
      }
      results.max
    }

    def amplify(program: String, input: Int): Int = amplify(program.split(",").toVector.map(_.toInt), input)

    def amplifyInputFile(): Int = {
      val inputFile = "data" / "day7" / "part1" / "input.txt"
      amplify(inputFile.contentAsString, 0)
    }
  }

}
