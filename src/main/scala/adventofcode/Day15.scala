package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._


object Day15 {


  class Code(val program:Vector[BigInt]) {
    def change(address: Int, value: BigInt):Code = new Code(program.updated(0, 2))

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

  def stringToCode(program: String): Code = new Code(program.split(",").toVector.map(x => BigInt(x)))

  def fileToCode(inputFile: File = "data" / "day15" / "input.txt"): Code = {
    stringToCode(inputFile.contentAsString)
  }





  object Part1 {

    object ProgramActor {

      sealed trait ProgramMessage

      case class Setup(outputOpt: Option[ActorRef[SearchBotActor.Control]], lastResponseToOpt: Option[ActorRef[SearchBotActor.Control]]) extends ProgramMessage

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
        outputToOpt: Option[ActorRef[SearchBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[SearchBotActor.Control]],
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
        outputToOpt: Option[ActorRef[SearchBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[SearchBotActor.Control]],
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
            //outputToOpt.foreach{outputTo => outputTo ! SearchBotActor.NeedInput}
            ProgramActor.askInput(context, code, outputToOpt, lastResponseToOpt, pointer, inputs, outputs, relativeBase) // request for more inputs
          case 3 =>
            val newCode = code.write(pointer + 1, relativeBase, firstParamMode, inputs.head)
            process(context, newCode, outputToOpt, lastResponseToOpt, pointer + 2, inputs.tail, outputs, relativeBase)
          case 4 =>
            val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
            outputToOpt.foreach { outputTo =>
              outputTo ! SearchBotActor.Output(outputValue)
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
            lastResponseToOpt.foreach { lastResponseTo => lastResponseTo ! SearchBotActor.Result(outputs, code, context.self) }
            Behaviors.stopped
        }
      }
    }

    sealed trait Tile

    object Unknown extends Tile {
      override def toString = " "
    }

    object Wall extends Tile {
      override def toString = "#"
    }

    object Explored extends Tile {
      override def toString = "."
    }

    object Oxygen extends Tile {
      override def toString = "O"
    }

    sealed trait Direction {
      val code:Int
      val reverse:Direction
    }
    object Start extends Direction {
      val code=0
      val reverse = Start
    }
    object Up extends Direction {
      val code=1
      val reverse = Down
    }
    object Down extends Direction {
      val code=2
      val reverse = Up
    }
    object Left extends Direction {
      val code=3  // West
      val reverse = Right
    }
    object Right extends Direction {
      val code=4  // East
      val reverse = Left
    }

    type Position=(Int,Int)
    def move(position:Position)(direction: Direction):Position = position match {
      case (x, y) =>
        direction match {
          case Up => (x, y - 1)
          case Left => (x - 1, y)
          case Down => (x, y + 1)
          case Right => (x + 1, y)
        }
    }

    case class Land(zones: Vector[Vector[Tile]]) {
      def alreadyExplored(position: (Int, Int)): Boolean = get(position) != Unknown

      val width = zones.head.size
      val height = zones.size

      override def toString() = zones.map(_.mkString).mkString("\n")

      def get(pos:Position): Tile = pos match {case (x,y) => zones(y)(x)}

      def set(pos:Position, tile: Tile): Land = pos match {case (x,y) =>Land(zones.updated(y, zones(y).updated(x, tile)))}
    }

    object Land {
      def apply(width: Int, height: Int): Land = {
        Land(Vector.fill(height)(Vector.fill(width)(Unknown)))
      }
    }




    object SearchBotActor {

      sealed trait Control

      case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control
      case class Output(value: BigInt) extends Control
      object NeedInput extends Control

      case class DirectionToCheck(
        position:Position,
        distanceWalked:Int=0,
        comingFrom:Direction,
        directions:List[Direction]=List(Up,Down,Left,Right)
      )

      def walk(
        programActor: ActorRef[ProgramActor.Input],
        listenActor: ActorRef[ListenActor.Response],
        land: Land,
        walkedPath:List[DirectionToCheck]
      ): Behavior[Control] = {
        println(land.toString())
        println("---------")
        walkedPath match {
          // --- already explored
          case DirectionToCheck(pos,dist,comingFrom,direction::remainingDirections)::tail if land.alreadyExplored(move(pos)(direction)) =>
            val newWalkedTile = DirectionToCheck(pos,dist,comingFrom,remainingDirections)
            walk(programActor,listenActor,land, newWalkedTile::tail)
          // --- new to check
          case DirectionToCheck(pos,dist,comingFrom,direction::remainingDirections)::tail =>
            val updatedWalkedTile = DirectionToCheck(pos,dist,comingFrom,remainingDirections)
            val newPos = move(pos)(direction)
            val newDist = dist + 1
            programActor ! ProgramActor.Input(direction.code)
            Behaviors.receiveMessage{
              case response:Output if response.value == 0 => // WALL
                walk(programActor,listenActor, land.set(newPos, Wall), updatedWalkedTile::tail)
              case response:Output if response.value == 1 => // MOVED
                val directions = List(Left,Right,Up,Down)
                val newWalkedTile = DirectionToCheck(newPos, newDist,direction,directions.filterNot(_ == direction.reverse))
                walk(programActor,listenActor, land.set(newPos, Explored), newWalkedTile::updatedWalkedTile::tail)
              case response:Output if response.value == 2 => // OXYGEN
                val newLand = land.set(newPos, Oxygen)
                println(newLand.toString())
                listenActor ! ListenActor.Response(newDist)
                Behaviors.stopped
            }
          // --- going back
          case directionToCheck::tail =>
            programActor ! ProgramActor.Input(directionToCheck.comingFrom.reverse.code)
            Behaviors.receiveMessage {
              case response:Output =>
                // no need to control as we're going back
                walk(programActor,listenActor,land,tail)
            }
        }
      }

      def apply(
        code: Code,
        listenActor: ActorRef[ListenActor.Response]
      ): Behavior[Control] = Behaviors.setup { context =>
        val programActor = context.spawn(ProgramActor(code), "program")
        programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))
        val width = 100
        val height = 100
        val startPosition = (width / 2, height / 2)
        val land = Land(width, height).set(startPosition,Explored)
        walk(programActor, listenActor, land, DirectionToCheck(startPosition,0,Start)::Nil)
      }
    }

    object ListenActor {
      case class Response(distance: Int)
    }
  }

  object Part2 {

  }
}
