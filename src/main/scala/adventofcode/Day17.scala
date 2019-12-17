package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

import scala.annotation.tailrec

object Day17 {

  class Code(val program: Vector[BigInt]) {

    private def updated(pointer: Int, value: BigInt): Code = {
      val adjustedProgram =
        if (pointer < program.size) program
        else program ++ Vector.fill[BigInt](pointer - program.size + 1)(BigInt(0))
      val newProgram = adjustedProgram.updated(pointer, value)
      new Code(newProgram)
    }

    def opcode(pointer: Int): BigInt = {
      apply(pointer)
    }

    private def apply(pointer: Int): BigInt = {
      if (pointer >= program.size) 0
      else
        program(pointer)
    }

    def read(codePos: Int, relativeBase: Int, mode: Int): BigInt = {
      mode match {
        case 0 => apply(program(codePos).toInt) // address mode
        case 1 => apply(codePos) // immediate mode
        case 2 => apply(relativeBase + apply(codePos).toInt) // relative mode
      }
    }

    def write(codePos: Int, relativeBase: Int, mode: Int, value: BigInt): Code = {
      mode match {
        case 0 => updated(apply(codePos).toInt, value) // address mode (position mode)
        case 1 => updated(codePos, value) // immediate mode
        case 2 => updated(relativeBase + apply(codePos).toInt, value) // relative mode
      }
    }
  }

  def stringToCode(program: String): Code = new Code(program.split(",").toVector.map(x => BigInt(x)))

  def fileToCode(inputFile: File = "data" / "day17" / "input.txt"): Code = {
    stringToCode(inputFile.contentAsString)
  }


  object ProgramActor {

    sealed trait ProgramMessage

    case class Setup(outputOpt: Option[ActorRef[AftScaffoldingControlActor.Control]], lastResponseToOpt: Option[ActorRef[AftScaffoldingControlActor.Control]]) extends ProgramMessage

    case class Input(value: BigInt) extends ProgramMessage

    def apply(code: Code): Behavior[ProgramMessage] = Behaviors.setup { context => {
      Behaviors.receiveMessage {
        case setup: Setup =>
          process(
            context = context,
            code = code,
            outputToOpt = setup.outputOpt,
            lastResponseToOpt = setup.lastResponseToOpt,
            pointer = 0,
            inputs = Vector.empty,
            outputs = Vector.empty,
            relativeBase = 0
          )
      }
    }
    }

    def askInput(
      context: ActorContext[ProgramMessage],
      code: Code,
      outputToOpt: Option[ActorRef[AftScaffoldingControlActor.Control]],
      lastResponseToOpt: Option[ActorRef[AftScaffoldingControlActor.Control]],
      pointer: Int,
      inputs: Vector[BigInt],
      outputs: Vector[BigInt],
      relativeBase: Int
    ): Behavior[ProgramMessage] =
      Behaviors.receiveMessage { case message: Input =>
        process(context, code, outputToOpt, lastResponseToOpt, pointer, inputs :+ message.value, outputs, relativeBase)
      }

    def process(
      context: ActorContext[ProgramMessage],
      code: Code,
      outputToOpt: Option[ActorRef[AftScaffoldingControlActor.Control]],
      lastResponseToOpt: Option[ActorRef[AftScaffoldingControlActor.Control]],
      pointer: Int,
      inputs: Vector[BigInt],
      outputs: Vector[BigInt],
      relativeBase: Int
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
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs, relativeBase)
        case 2 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam * secondParam)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs, relativeBase)
        case 3 if inputs.isEmpty =>
          ProgramActor.askInput(context, code, outputToOpt, lastResponseToOpt, pointer, inputs, outputs, relativeBase) // request for more inputs
        case 3 =>
          val newCode = code.write(pointer + 1, relativeBase, firstParamMode, inputs.head)
          process(context, newCode, outputToOpt, lastResponseToOpt, pointer + 2, inputs.tail, outputs, relativeBase)
        case 4 =>
          val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
          outputToOpt.foreach { outputTo =>
            outputTo ! AftScaffoldingControlActor.Output(outputValue)
          }
          process(context, code, outputToOpt, lastResponseToOpt, pointer + 2, inputs, outputs :+ outputValue, relativeBase)
        case 5 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          if (firstParam != 0)
            process(context, code, outputToOpt, lastResponseToOpt, secondParam.toInt, inputs, outputs, relativeBase)
          else
            process(context, code, outputToOpt, lastResponseToOpt, pointer + 3, inputs, outputs, relativeBase)
        case 6 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          if (firstParam == 0)
            process(context, code, outputToOpt, lastResponseToOpt, secondParam.toInt, inputs, outputs, relativeBase)
          else
            process(context, code, outputToOpt, lastResponseToOpt, pointer + 3, inputs, outputs, relativeBase)
        case 7 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam < secondParam) 1 else 0)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs, relativeBase)
        case 8 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam == secondParam) 1 else 0)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs, relativeBase)
        case 9 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val newRelativeBase = relativeBase + firstParam.toInt
          process(context, code, outputToOpt, lastResponseToOpt, pointer + 2, inputs, outputs, newRelativeBase)
        case 99 =>
          lastResponseToOpt.foreach { lastResponseTo => lastResponseTo ! AftScaffoldingControlActor.Result(outputs, code, context.self) }
          Behaviors.stopped
      }
    }
  }

  sealed trait Tile

  object OpenSpace extends Tile {
    override def toString = "."
  }
  object Scaffold extends Tile {
    override def toString = "#"
  }
  object Intersection extends Tile {
    override def toString = "O"
  }
  object BotUp extends Tile {
    override def toString = "^"
  }
  object BotDown extends Tile {
    override def toString = "v"
  }
  object BotLeft extends Tile {
    override def toString = "<"
  }
  object BotRight extends Tile {
    override def toString = ">"
  }

  type Position = (Int,Int)

  case class Land(zones: Vector[Vector[Tile]]) {
    val width = zones.head.size
    val height = zones.size

    override def toString() = zones.map(_.mkString).mkString("\n")

    def get(pos: Position): Tile = pos match {
      case (x, y) => zones(y)(x)
    }

    def set(pos: Position, tile: Tile): Land = pos match {
      case (x, y) => Land(zones.updated(y, zones(y).updated(x, tile)))
    }
  }

  object Land {
    def apply(width: Int, height: Int): Land = {
      Land(Vector.fill(height)(Vector.fill(width)(OpenSpace)))
    }
  }





  object AftScaffoldingControlActor {

    sealed trait Control

    case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control

    case class Output(value: BigInt) extends Control

    object NeedInput extends Control

    def buildLand(programActor: ActorRef[ProgramActor.ProgramMessage], listenActor: ActorRef[ListenActor.Response], landTiles: Vector[Vector[Tile]]): Behavior[Control] = {
      Behaviors.receiveMessage {
        case response:Output if response.value == 10 && landTiles.last.isEmpty =>
          val finalBuiltLand = landTiles.init
          println(finalBuiltLand.toString())
          Behaviors.same // TODO
        case response:Output if response.value == 10 =>
          val newLandTiles = landTiles.appended(Vector.empty[Tile])
          buildLand(programActor, listenActor, newLandTiles)
        case response:Output =>
          val tile = response.value.toChar match {
            case '.' => OpenSpace
            case '#' => Scaffold
            case '^' => BotUp
            case 'v' => BotDown
            case '<' => BotLeft
            case '>' => BotRight
          }
          val newLandTiles = landTiles.init :+ (landTiles.last:+tile)
          buildLand(programActor, listenActor, newLandTiles)
        case x =>
          println(s"NOT UNDERSTOOD OUTPUT $x")
          Behaviors.same
      }
    }

    def apply(
      code: Code,
      listenActor: ActorRef[ListenActor.Response]
    ): Behavior[Control] = Behaviors.setup { context =>
      val programActor = context.spawn(ProgramActor(code), "program")
      programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))

      buildLand(programActor, listenActor, Vector(Vector.empty[Tile]))
    }
  }

  object ListenActor {

    case class Response(value: Int)

  }

}
