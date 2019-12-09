package adventofcode

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import better.files._


object Day9 {

  object Part1 {

    class Code(program:Vector[BigInt]) {
      def updated(pointer: Int, value: BigInt):Code = {
        val adjustedProgram =
          if (pointer < program.size) program
          else program++Vector.fill[BigInt](pointer-program.size+1)(BigInt(0))
        val newProgram = adjustedProgram.updated(pointer, value)
        new Code(newProgram)
      }
      def apply(pointer:Int):BigInt = program(pointer)

      def read(codePos: Int, relativeBase:Int, mode: Int): BigInt = {
        mode match {
          case 0 => program(program(codePos).toInt) // address mode
          case 1 => program(codePos) // immediate mode
          case 2 => program(relativeBase) // relative mode
        }
      }

      def write(codePos: Int, relativeBase:Int, mode: Int, value: BigInt): Code = {
        mode match {
          case 0 => updated(program(codePos).toInt, value) // address mode
          case 1 => updated(codePos, value) // immediate mode
          case 2 => updated(relativeBase, value)// relative mode
        }
      }

    }


    object ProgramActor {
      sealed trait ProgramMessage
      case class Setup(linkedToOpt:Option[ActorRef[Input]], lastResponseTo:ActorRef[EngineActor.Control]) extends ProgramMessage
      case class Input(value:BigInt) extends ProgramMessage

      def apply(code: Code):Behavior[ProgramMessage] = Behaviors.setup { context => {
          Behaviors.receiveMessage {
            case setup: Setup =>
              process(
                context = context,
                code = code,
                linkedToOpt = setup.linkedToOpt,
                lastResponseTo = setup.lastResponseTo,
                pointer = 0,
                inputs = Vector.empty,
                lastOutput = None,
                relativeBase = 0
              )
          }
        }
      }

      def askInput(
        context:ActorContext[ProgramMessage],
        code: Code,
        linkedToOpt:Option[ActorRef[Input]],
        lastResponseTo:ActorRef[EngineActor.Control],
        pointer:Int,
        inputs:Vector[BigInt],
        lastOutput:Option[BigInt]=None,
        relativeBase:Int=0
      ): Behavior[ProgramMessage] =
          Behaviors.receiveMessage { case message:Input =>
            process(context, code, linkedToOpt, lastResponseTo, pointer, inputs :+ message.value, lastOutput, relativeBase)
        }

      def process(
        context:ActorContext[ProgramMessage],
        code: Code,
        linkedToOpt:Option[ActorRef[Input]],
        lastResponseTo:ActorRef[EngineActor.Control],
        pointer:Int,
        inputs:Vector[BigInt],
        lastOutput:Option[BigInt],
        relativeBase:Int
      ): Behavior[ProgramMessage] = {
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
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam + secondParam)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput,relativeBase)
          case 2 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam * secondParam)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput,relativeBase)
          case 3 if inputs.isEmpty =>
            ProgramActor.askInput(context, code, linkedToOpt, lastResponseTo, pointer, inputs, lastOutput,relativeBase) // request for more inputs
          case 3 =>
            val address = code(pointer + 1)
            val extendedProgram = code
            val newCode = extendedProgram.updated(address.toInt, inputs.head)
            process(context, newCode, linkedToOpt, lastResponseTo, pointer + 2, inputs.tail, lastOutput,relativeBase)
          case 4 =>
            val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
            linkedToOpt.foreach { linkedTo =>
              linkedTo ! Input(outputValue)
            }
            process(context, code, linkedToOpt, lastResponseTo, pointer + 2, inputs, Some(outputValue),relativeBase)
          case 5 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            if (firstParam != 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, lastOutput,relativeBase)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, lastOutput,relativeBase)
          case 6 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            if (firstParam == 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, lastOutput,relativeBase)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, lastOutput,relativeBase)
          case 7 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam < secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput,relativeBase)
          case 8 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam == secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, lastOutput,relativeBase)
          case 9 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val newRelativeBase = relativeBase + firstParam.toInt
            process(context, code, linkedToOpt, lastResponseTo, pointer + 2, inputs, lastOutput, newRelativeBase)
          case 99 =>
            lastResponseTo ! EngineActor.Result(lastOutput, code, context.self)
            Behaviors.stopped
        }
      }

    }


    object EngineActor {
      sealed trait Control
      case class Start(code:Code, value:BigInt) extends Control
      case class Result(value:Option[BigInt], code:Code, from:ActorRef[ProgramActor.ProgramMessage]) extends Control

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



    def stringToCode(program:String):Code = new Code(program.split(",").toVector.map(x => BigInt(x)))

    def fileToCode(): Code = {
      val inputFile = "data" / "day9" / "part1" / "input.txt"
      stringToCode(inputFile.contentAsString)
    }
  }



  // ========================================================================================

  object Part2 {

  }

}
