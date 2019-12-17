package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._

import scala.annotation.tailrec

object Day17 {

  class Code(val program: Vector[BigInt]) {

    def change(address: Int, value: BigInt):Code = new Code(program.updated(address, value))

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
          println("FINISHED - 99")
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
  trait Bot extends Tile {
    val direction:Direction
  }
  object BotUp extends Bot {
    override def toString = "^"
    override val direction: Direction = Up
  }
  object BotDown extends Bot {
    override def toString = "v"
    override val direction: Direction = Down
  }
  object BotLeft extends Bot {
    override def toString = "<"
    override val direction: Direction = Left
  }
  object BotRight extends Bot {
    override def toString = ">"
    override val direction: Direction = Right
  }

  type Position = (Int,Int)

  sealed trait Direction {
    val reverse: Direction
    val turnLeft: Direction
    val turnRight: Direction
  }

  object Up extends Direction {
    val reverse = Down
    val turnLeft = Left
    val turnRight = Right
  }

  object Down extends Direction {
    val reverse = Up
    val turnLeft = Right
    val turnRight = Left
  }

  object Left extends Direction {
    val reverse = Right
    val turnLeft = Down
    val turnRight = Up
  }

  object Right extends Direction {
    val reverse = Left
    val turnLeft = Up
    val turnRight = Down
  }

  val directions = List(Up,Down,Left,Right)


  def move(position: Position)(direction: Direction): Position = position match {
    case (x, y) =>
      direction match {
        case Up => (x, y - 1)
        case Left => (x - 1, y)
        case Down => (x, y + 1)
        case Right => (x + 1, y)
      }
  }


  case class Land(zones: Vector[Vector[Tile]]) {
    val width = zones.head.size
    val height = zones.size

    override def toString() = zones.map(_.mkString).mkString("\n")

    def get(pos: Position): Tile = pos match {
      case (x, y) => zones(y)(x)
    }

    def safeGet(pos:Position):Option[Tile] = pos match {
      case (x,y) if x < 0 || y < 0 || x > width-1 || y > height-1 => None
      case (x,y) => Some(zones(y)(x))
    }

    def set(pos: Position, tile: Tile): Land = pos match {
      case (x, y) => Land(zones.updated(y, zones(y).updated(x, tile)))
    }

    def search(criteria: Tile=>Boolean):IndexedSeq[(Tile,Position)] = {
      for {
        x <- 0 until width
        y <- 0 until height
        pos = (x, y)
        tile = get(pos)
        if criteria(tile)
      } yield tile->pos
    }
    def intersections():List[Position] = {
      val results = for {
        x <- 1 until width-1
        y <- 1 until height-1
        pos = (x,y)
        if get(pos) == Scaffold && directions.forall{direction => get(move(pos)(direction))  == Scaffold}
      } yield pos
      results.toList
    }
    def botPosition():Option[(Tile,Position)] = {
      search(tile => tile != OpenSpace && tile!=Scaffold).headOption
    }
  }

  object Land {
    def charToTile(value: Char):Tile = value match {
      case '.' => OpenSpace
      case '#' => Scaffold
      case '^' => BotUp
      case 'v' => BotDown
      case '<' => BotLeft
      case '>' => BotRight
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



  sealed trait Turn
  object TurnLeft extends Turn {
    override def toString: String = "L"
  }
  object TurnRight extends Turn {
    override def toString: String = "R"
  }
  case class MoveInstruction(turn:Turn,moveCount:Int) {
    override def toString: String = s"$turn$moveCount"
  }

  def fullPath(land:Land):List[MoveInstruction] = {
    land.botPosition() match {
      case None => List.empty
      case Some((botTile:Bot, initialPosition)) =>
        def walker(direction:Direction, position:Position, pathAccumulator:List[MoveInstruction]):List[MoveInstruction] = {
          def movesSimulation(direction:Direction, turned:Turn): Option[(Position, MoveInstruction)] = {
            var count = 0
            var currentPosition = position
            while (land.safeGet(move(currentPosition)(direction)) == Some(Scaffold)) {
              currentPosition = move(currentPosition)(direction)
              count += 1
            }
            if (count==0) None else Some(currentPosition->MoveInstruction(turned, count))
          }
          val leftForward = movesSimulation(direction.turnLeft, TurnLeft)
          val rightForward = movesSimulation(direction.turnRight, TurnRight)
          leftForward.orElse(rightForward) match {
            case None => pathAccumulator.reverse // Finished
            case Some((newPosition, instruction)) =>
              val newDirection = if (instruction.turn == TurnLeft) direction.turnLeft else direction.turnRight
              walker(newDirection, newPosition, instruction::pathAccumulator)
          }
        }
        walker(botTile.direction,initialPosition, Nil)
    }
  }





  object AftScaffoldingControlActor {

    sealed trait Control

    case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control

    case class Output(value: BigInt) extends Control

    object NeedInput extends Control


    def part2(programActor: ActorRef[ProgramActor.ProgramMessage], listenActor: ActorRef[ListenActor.Response], land: Land): Behavior[Control] = {
      val path = fullPath(land)
      println(path.mkString(","))
      // Add huffman inspired algorithm to solve the issue
      ???
    }

    def buildLand(programActor: ActorRef[ProgramActor.ProgramMessage], listenActor: ActorRef[ListenActor.Response], landTiles: Vector[Vector[Tile]]): Behavior[Control] = {
      Behaviors.receiveMessage {
        case response:Output if response.value == 10 && landTiles.last.isEmpty =>
          val land = Land(landTiles.init)
          println(land)
          val value = land.intersections().map{case (x,y) => x*y}.sum
          listenActor ! ListenActor.Response(value)

          part2(programActor, listenActor, land)
        case response:Output if response.value == 10 =>
          val newLandTiles = landTiles.appended(Vector.empty[Tile])
          buildLand(programActor, listenActor, newLandTiles)
        case response:Output =>
          val tile = Land.charToTile(response.value.toChar)
          val newLandTiles = landTiles.init :+ (landTiles.last:+tile)
          buildLand(programActor, listenActor, newLandTiles)
        case response:Result =>
          println(s"FINISHED")
          Behaviors.stopped
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
