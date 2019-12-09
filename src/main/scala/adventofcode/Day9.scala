package adventofcode

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import better.files._


object Day9 {


    class Code(val program:Vector[BigInt]) {
      private def updated(pointer: Int, value: BigInt):Code = {
        val adjustedProgram =
          if (pointer < program.size) program
          else program++Vector.fill[BigInt](pointer-program.size+1)(BigInt(0))
        val newProgram = adjustedProgram.updated(pointer, value)
        new Code(newProgram)
      }
      def opcode(pointer:Int):BigInt = {
        apply(pointer)
      }
      private def apply(pointer:Int):BigInt = {
        if (pointer >= program.size) 0
        else
          program(pointer)
      }

      def read(codePos: Int, relativeBase:Int, mode: Int): BigInt = {
        mode match {
          case 0 => apply(program(codePos).toInt) // address mode
          case 1 => apply(codePos) // immediate mode
          case 2 => apply(relativeBase+apply(codePos).toInt) // relative mode
        }
      }

      def write(codePos: Int, relativeBase:Int, mode: Int, value: BigInt): Code = {
        mode match {
          case 0 => updated(apply(codePos).toInt, value) // address mode (position mode)
          case 1 => updated(codePos, value) // immediate mode
          case 2 => updated(relativeBase+apply(codePos).toInt, value)// relative mode
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
                outputs = Vector.empty,
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
        outputs:Vector[BigInt],
        relativeBase:Int
      ): Behavior[ProgramMessage] =
          Behaviors.receiveMessage { case message:Input =>
            process(context, code, linkedToOpt, lastResponseTo, pointer, inputs :+ message.value, outputs, relativeBase)
        }

      def process(
        context:ActorContext[ProgramMessage],
        code: Code,
        linkedToOpt:Option[ActorRef[Input]],
        lastResponseTo:ActorRef[EngineActor.Control],
        pointer:Int,
        inputs:Vector[BigInt],
        outputs:Vector[BigInt],
        relativeBase:Int
      ): Behavior[ProgramMessage] = {
        val instruction = code.opcode(pointer).toString
        val opcode = instruction.reverse.take(2).reverse.toInt
        val (firstParamMode, secondParamMode, thirdParamMode) = instruction.reverse.drop(2).toVector.map(_.toInt - 48) match {
          case Vector() => (0, 0, 0)
          case Vector(a) => (a, 0, 0)
          case Vector(a, b) => (a, b, 0)
          case Vector(a, b, c) => (a, b, c) // !!!! I MADE A BIG MISTAKE PREVIOUSLY HERE, (c,b,a) instead of (a,b,c) => LOST SEVERAL HOURS ON THIS BUG !!!
        }
        //println(s"$opcode $firstParamMode-$secondParamMode-$thirdParamMode")
        opcode match {
          case 1 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam + secondParam)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, outputs,relativeBase)
          case 2 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam * secondParam)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, outputs,relativeBase)
          case 3 if inputs.isEmpty =>
            ProgramActor.askInput(context, code, linkedToOpt, lastResponseTo, pointer, inputs, outputs,relativeBase) // request for more inputs
          case 3 =>
            //val address = code.read(pointer + 1, relativeBase, firstParamMode)
            val newCode = code.write(pointer+1, relativeBase, firstParamMode, inputs.head)
            process(context, newCode, linkedToOpt, lastResponseTo, pointer + 2, inputs.tail, outputs,relativeBase)
          case 4 =>
            val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
            linkedToOpt.foreach { linkedTo =>
              linkedTo ! Input(outputValue)
            }
            process(context, code, linkedToOpt, lastResponseTo, pointer + 2, inputs, outputs:+outputValue,relativeBase)
          case 5 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            if (firstParam != 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, outputs,relativeBase)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, outputs,relativeBase)
          case 6 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            if (firstParam == 0)
              process(context, code, linkedToOpt, lastResponseTo, secondParam.toInt, inputs, outputs,relativeBase)
            else
              process(context, code, linkedToOpt, lastResponseTo, pointer + 3, inputs, outputs,relativeBase)
          case 7 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam < secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, outputs,relativeBase)
          case 8 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
            val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam == secondParam) 1 else 0)
            process(context, newProgram, linkedToOpt, lastResponseTo, pointer + 4, inputs, outputs,relativeBase)
          case 9 =>
            val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
            val newRelativeBase = relativeBase + firstParam.toInt
            process(context, code, linkedToOpt, lastResponseTo, pointer + 2, inputs, outputs, newRelativeBase)
          case 99 =>
            lastResponseTo ! EngineActor.Result(outputs, code, context.self)
            Behaviors.stopped
        }
      }

    }


    object EngineActor {
      sealed trait Control
      case class Start(code:Code, value:BigInt) extends Control
      case class Result(outputs:Vector[BigInt], code:Code, from:ActorRef[ProgramActor.ProgramMessage]) extends Control {
        def latestOutput = outputs.lastOption
      }

      def apply():Behavior[Control] = Behaviors.setup{ context =>
        Behaviors.receiveMessage {
          case Start(code, value) =>
            val actorName = "program"
            val programActor = context.spawn(ProgramActor(code), actorName)
            programActor ! ProgramActor.Setup(None, context.self)
            programActor ! ProgramActor.Input(value)

            Behaviors.receiveMessage{
              case result:Result =>
                Behaviors.stopped
            }
        }
      }
    }



    def stringToCode(program:String):Code = new Code(program.split(",").toVector.map(x => BigInt(x)))

    def fileToCode(inputFile: File = "data" / "day9" / "part1" / "input.txt"): Code = {
      stringToCode(inputFile.contentAsString)
    }




}
