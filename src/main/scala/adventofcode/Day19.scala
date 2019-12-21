package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

import scala.annotation.tailrec

object Day19 {

  class Code(val program: Vector[BigInt]) {

    def change(address: Int, value: BigInt): Code = new Code(program.updated(address, value))

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

  def fileToCode(inputFile: File = "data" / "day19" / "input.txt"): Code = {
    stringToCode(inputFile.contentAsString)
  }


  object ProgramActor {

    sealed trait ProgramMessage

    case class Setup(outputOpt: Option[ActorRef[BeamControlActor.Control]], lastResponseToOpt: Option[ActorRef[BeamControlActor.Control]]) extends ProgramMessage

    case class Input(value: BigInt) extends ProgramMessage

    var initialCode:Code=_
    def apply(code: Code): Behavior[ProgramMessage] = Behaviors.setup { context => {
      initialCode = code
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
      outputToOpt: Option[ActorRef[BeamControlActor.Control]],
      lastResponseToOpt: Option[ActorRef[BeamControlActor.Control]],
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
      outputToOpt: Option[ActorRef[BeamControlActor.Control]],
      lastResponseToOpt: Option[ActorRef[BeamControlActor.Control]],
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
            outputTo ! BeamControlActor.Output(outputValue)
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
          //lastResponseToOpt.foreach { lastResponseTo => lastResponseTo ! BeamControlActor.Result(outputs, code, context.self) }
          //println("FINISHED - 99")
          //Behaviors.stopped
          // REBOOT :
          process(context, initialCode, outputToOpt, lastResponseToOpt, 0, Vector.empty, Vector.empty, 0)
      }
    }
  }

  sealed trait Tile

  object OpenSpace extends Tile {
    override def toString = "."
  }

  object Affected extends Tile {
    override def toString = "#"
  }

  type Position = (Int, Int)

  case class Land(zones: Vector[Vector[Tile]]) {
    val width = zones.head.size
    val height = zones.size

    override def toString() = zones.map(_.mkString).mkString("\n")

    def get(pos: Position): Tile = pos match {
      case (x, y) => zones(y)(x)
    }

    def safeGet(pos: Position): Option[Tile] = pos match {
      case (x, y) if x < 0 || y < 0 || x > width - 1 || y > height - 1 => None
      case (x, y) => Some(zones(y)(x))
    }

    def set(pos: Position, tile: Tile): Land = pos match {
      case (x, y) => Land(zones.updated(y, zones(y).updated(x, tile)))
    }

    def search(criteria: Tile => Boolean): IndexedSeq[(Tile, Position)] = {
      for {
        x <- 0 until width
        y <- 0 until height
        pos = (x, y)
        tile = get(pos)
        if criteria(tile)
      } yield tile -> pos
    }
  }

  object Land {
    def charToTile(value: Char): Tile = value match {
      case '.' => OpenSpace
      case '#' => Affected
    }

    def apply(width: Int, height: Int): Land = {
      Land(Vector.fill(height)(Vector.fill(width)(OpenSpace)))
    }

    def apply(landString: String): Land = {
      Land(
        landString
          .split("\n")
          .toVector
          .map(row => row.toCharArray.toVector.map(charToTile))
      )
    }
  }


  object BeamControlActor {

    sealed trait Control

    case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control

    case class Output(value: BigInt) extends Control

    object NeedInput extends Control

    case class HorizontalSlice(xStart:Int,xEnd:Int) {
      def width = xEnd-xStart+1
    }

    def checkBeamSlides(squareSize: Option[Int], y:Int, slices: List[HorizontalSlice]): Option[Int] = {
      squareSize.flatMap { size =>
        slices.drop(size - 1).headOption.flatMap {
          case upperSlice =>
            val bottomSlice = slices.head
            val uxs = upperSlice.xStart
            val uxe = upperSlice.xEnd
            val bxs = bottomSlice.xStart
            val bxe = bottomSlice.xEnd
            if ( (uxs <= bxs) && (bxs + size - 1  <= uxe) ) {
              val fx = bxs
              val fy = y - size + 1
              System.err.print(s"<$fx $fy>")
              Some(fx * 10000 + fy)
            }
            else None
        }
      }
    }

    val debug=false

    private def checkForResults(listenActor: ActorRef[ListenActor.Response], squareSize: Option[Int], h:Int, y: Int, count: Int, value: BigInt, newBeamHorizontalSlices: List[HorizontalSlice]) = {
      if (squareSize.isDefined) checkBeamSlides(squareSize, y, newBeamHorizontalSlices).foreach { case result => listenActor ! ListenActor.Response(result)}
      else if (y == h - 1) listenActor ! ListenActor.Response(count + value.toInt)

    }
    def searchCount(
      programActor: ActorRef[ProgramActor.ProgramMessage],
      listenActor: ActorRef[ListenActor.Response],
      squareSize: Option[Int],
      beamHorizontalSlices:List[HorizontalSlice],
      x: Int, y: Int,
      w: Int, h: Int,
      count:Int = 0,
      prevLineCount:Int=0,
      xstart:Int= 0
    ): Behavior[Control] = {
      programActor ! ProgramActor.Input(x)
      programActor ! ProgramActor.Input(y)
      Behaviors.receiveMessage {
        //------------------------------------
        case Output(value) if value == 0 & count > prevLineCount => // Line End, exists from beam zone
          if (debug) print(".\n"+("."*xstart))
          var newSlices = HorizontalSlice(xstart,x-1)::beamHorizontalSlices
          if (y%1000==0) {
            newSlices = newSlices.take(110)
          } // TODO HARDCODED 110 FOR 100
          checkForResults(listenActor, squareSize, h, y, count, value, newSlices)
          if (y==h-1) Behaviors.stopped
          else searchCount(programActor, listenActor, squareSize, newSlices, xstart, y+1, w, h, count, count, xstart)
        //------------------------------------
        case Output(value) if x==w-1 => // Line End, beam rich zone
          if (debug) {if (value==1) print("*\n"+("."*xstart)) else print(".\n"+("."*xstart))}
          var newSlices = HorizontalSlice(xstart,if (value==1) x else x-1)::beamHorizontalSlices
          if (y%1000==0) {
            newSlices = newSlices.take(110)
          } // TODO HARDCODED 110 FOR 100
          checkForResults(listenActor, squareSize, h, y, count, value, newSlices)
          if (y==h-1) Behaviors.stopped
          else searchCount(programActor, listenActor, squareSize, newSlices, xstart, y+1, w, h, count+value.toInt, count+value.toInt, xstart)
        //------------------------------------
        case Output(value) if value==0 => // Line begins, didn't reached the beam area
          if (debug) print(".")
          searchCount(programActor, listenActor, squareSize, beamHorizontalSlices, x+1, y, w, h, count, prevLineCount, x+1)
        //------------------------------------
        case Output(_) => // in beam area
          if (debug) print("*")
          searchCount(programActor, listenActor, squareSize, beamHorizontalSlices, x+1, y, w, h, count+1, prevLineCount, xstart)
      }
    }


    def justDump(
      programActor: ActorRef[ProgramActor.ProgramMessage],
      listenActor: ActorRef[ListenActor.Response],
      squareSize: Option[Int],
      beamHorizontalSlices:List[HorizontalSlice],
      x: Int, y: Int,
      w: Int, h: Int
    ): Behavior[Control] = {
      programActor ! ProgramActor.Input(x)
      programActor ! ProgramActor.Input(y)
      Behaviors.receiveMessage {
        //------------------------------------
        case Output(value) if x==w-1 => // Line End
          if (debug) {if (value==1) println("*") else println(".")}
          if (y==h-1) Behaviors.stopped
          else justDump(programActor, listenActor, squareSize, beamHorizontalSlices, 0, y+1, w, h)
        //------------------------------------
        case Output(value)  =>
          if (debug) {if (value ==1) print("*") else print (".")}
          justDump(programActor, listenActor, squareSize, beamHorizontalSlices, x+1, y, w, h)
      }
    }





    def computeAffectedNextLine(
      programActor: ActorRef[ProgramActor.ProgramMessage],
      listenActor: ActorRef[ListenActor.Response],
      position: Position,
      areaLimit: Position,
      squareSize: Option[Int]
    ): Behavior[Control] = {
      areaLimit match {
        case (w, h) =>
          position match {
            case (xs, ys) =>
              searchCount(programActor, listenActor,squareSize, Nil, xs, ys, w, h)
              //justDump(programActor, listenActor,squareSize, Nil, xs, ys, w, h)
          }
      }
    }

    def apply(
      code: Code,
      listenActor: ActorRef[ListenActor.Response],
      areaLimit: Position,
      squareSize: Option[Int]
    ): Behavior[Control] = Behaviors.setup { context =>
      val programActor = context.spawn(ProgramActor(code), "program")
      programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))
      computeAffectedNextLine(programActor, listenActor, (0, 0), areaLimit, squareSize)
    }
  }

  object ListenActor {

    case class Response(value: Int)

  }

}
