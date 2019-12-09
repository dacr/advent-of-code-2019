package adventofcode

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import better.files._


object Day9 {

  object Part1 {

    object ProgramActor {
      sealed trait ProgramMessage
      case class Setup(linkedToOpt:Option[ActorRef[Input]], lastResponseTo:ActorRef[EngineActor.Control]) extends ProgramMessage
      case class Input(value:BigInt) extends ProgramMessage

      def apply(code: Vector[BigInt]):Behavior[ProgramMessage] = Behaviors.setup { context => {
          Behaviors.receiveMessage {
            case setup: Setup =>
              process(context, code, setup.linkedToOpt, setup.lastResponseTo, 0, Vector.empty, None)
          }
        }
      }

      def askInput(context:ActorContext[ProgramMessage], code: Vector[BigInt], linkedToOpt:Option[ActorRef[Input]], lastResponseTo:ActorRef[EngineActor.Control], pointer:Int=0, inputs:Vector[BigInt]=Vector.empty, lastOutput:Option[BigInt]=None): Behavior[ProgramMessage] =
          Behaviors.receiveMessage { case message:Input =>
            process(context, code, linkedToOpt, lastResponseTo, pointer, inputs :+ message.value, lastOutput)
        }

      def readValue(program: Vector[BigInt], paramPos: Int, paramMode: Int): BigInt = {
        paramMode match {
          case 0 => program(program(paramPos).toInt) // address mode
          case 1 => program(paramPos) // immediate mode
        }
      }

      def writeValue(program: Vector[BigInt], paramPos: Int, paramMode: Int, value: BigInt): Vector[BigInt] = {
        paramMode match {
          case 0 => program.updated(program(paramPos).toInt, value) // address mode
          case 1 => program.updated(paramPos, value) // immediate mode
        }
      }

      def process(context:ActorContext[ProgramMessage], code: Vector[BigInt], linkedToOpt:Option[ActorRef[Input]], lastResponseTo:ActorRef[EngineActor.Control], pointer:Int, inputs:Vector[BigInt], lastOutput:Option[BigInt]): Behavior[ProgramMessage] = {
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
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 2 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, firstParam * secondParam)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 3 if inputs.isEmpty =>
            ProgramActor.askInput(context, code, linkedToOpt, lastResponseTo, pointer, inputs, lastOutput) // request for more inputs
          case 3 =>
            val address = code(pointer + 1)
            val extendedProgram = code
            val newCode = extendedProgram.updated(address.toInt, inputs.head)
            process(context, newCode, linkedToOpt, lastResponseTo, pointer + 2, inputs.tail, lastOutput)
          case 4 =>
            val outputValue = readValue(code, pointer + 1, firstParamMode)
            linkedToOpt.foreach { linkedTo =>
              linkedTo ! Input(outputValue)
            }
            process(context, code, linkedToOpt, lastResponseTo, pointer + 2, inputs, Some(outputValue))
          case 5 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam != 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, lastOutput)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, lastOutput)
          case 6 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            if (firstParam == 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, lastOutput)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, lastOutput)
          case 7 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam < secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 8 =>
            val firstParam = readValue(code, pointer + 1, firstParamMode)
            val secondParam = readValue(code, pointer + 2, secondParamMode)
            val newProgram = writeValue(code, pointer + 3, thirdParamMode, if (firstParam == secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput)
          case 99 =>
            lastResponseTo ! EngineActor.Result(lastOutput, code, context.self)
            Behaviors.stopped
        }
      }
    }


    object EngineActor {
      sealed trait Control
      case class Start(code:Vector[BigInt], value:BigInt) extends Control
      case class Result(value:Option[BigInt], code:Vector[BigInt], from:ActorRef[ProgramActor.ProgramMessage]) extends Control

      def apply():Behavior[Control] = Behaviors.setup{ context =>
        Behaviors.receiveMessage {
          case Start(code, value) =>
            val actorName = "program"
            val programActor = context.spawn(ProgramActor(code), actorName)
            programActor ! ProgramActor.Setup(None, context.self)
            programActor ! ProgramActor.Input(value)

            Behaviors.receiveMessage{
              case Result(Some(value),finalCode, from) =>
                Behaviors.stopped
            }
        }
      }
    }



    def stringToCode(program:String):Vector[BigInt] = program.split(",").toVector.map(x => BigInt(x))

    def fileToCode(): Vector[BigInt] = {
      val inputFile = "data" / "day9" / "part1" / "input.txt"
      stringToCode(inputFile.contentAsString)
    }
  }



  // ========================================================================================

  object Part2 {

  }

}
