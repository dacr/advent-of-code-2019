package adventofcode

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import better.files._


object Day13 {


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

  def fileToCode(inputFile: File = "data" / "day13" / "input.txt"): Code = {
    stringToCode(inputFile.contentAsString)
  }





  object Part1 {

    object ProgramActor {

      sealed trait ProgramMessage

      case class Setup(outputOpt: Option[ActorRef[DrawBotActor.Control]], lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]]) extends ProgramMessage

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
        outputToOpt: Option[ActorRef[DrawBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]],
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
        outputToOpt: Option[ActorRef[DrawBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]],
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
              outputTo ! DrawBotActor.Output(outputValue)
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
            lastResponseToOpt.foreach { lastResponseTo => lastResponseTo ! DrawBotActor.Result(outputs, code, context.self) }
            Behaviors.stopped
        }
      }
    }

    sealed trait Tile

    object Empty extends Tile {
      override def toString = "Empty"
    }

    object Wall extends Tile {
      override def toString = "Wall"
    }

    object Block extends Tile {
      override def toString = "Block"
    }

    object Paddle extends Tile {
      override def toString = "Paddle"
    }

    object Ball extends Tile {
      override def toString = "Ball"
    }

    case class Screen(zones: Vector[Vector[Tile]]) {
      val width = zones.head.size
      val height = zones.size

      override def toString() = zones.map(_.mkString).mkString("\n")

      def get(x: Int, y: Int): Tile = zones(y)(x)

      def set(x: Int, y: Int, tile: Tile): Screen = Screen(zones.updated(y, zones(y).updated(x, tile)))

      def set(x: Int, y: Int, tileId: Int): Screen = {
        val tile = tileId match {
          case 0 => Empty
          case 1 => Wall
          case 2 => Block
          case 3 => Paddle
          case 4 => Ball
        }
        Screen(zones.updated(y, zones(y).updated(x, tile)))
      }

      def blocks(): Iterable[(Int, Int)] = {
        for {
          y <- 0 until height
          x <- 0 until width
          if get(x, y) == Block
        } yield (x, y)
      }

      def blocksCount(): Int = blocks.size
    }

    object Screen {
      def apply(width: Int, height: Int): Screen = {
        Screen(Vector.fill(height)(Vector.fill(width)(Empty)))
      }
    }


    object DrawBotActor {

      sealed trait Control

      case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control {
        def latestOutput = outputs.lastOption
      }

      case class Output(value: BigInt) extends Control

      def draw(programActor: ActorRef[ProgramActor.Input], listenActor: ActorRef[ListenActor.Response], screen: Screen): Behavior[Control] = {
        Behaviors.receiveMessage {
          case Output(x) =>
            Behaviors.receiveMessage {
              case Output(y) =>
                Behaviors.receiveMessage {
                  case Output(tileId) =>
                    val newScreen = screen.set(x.toInt, y.toInt, tileId.toInt)
                    draw(programActor, listenActor, newScreen)
                }
            }
          case result: Result =>
            listenActor ! ListenActor.Response(screen.blocksCount())
            Behaviors.stopped
        }
      }

      def apply(code: Code, listenActor: ActorRef[ListenActor.Response], startBlack: Boolean = true): Behavior[Control] = Behaviors.setup { context =>
        val programActor = context.spawn(ProgramActor(code), "program")
        programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))
        val screen = Screen(160, 140)
        val startPosition = (screen.width / 2, screen.height / 2)
        draw(programActor, listenActor, screen) // PART 1
      }
    }

    object ListenActor {
      case class Response(panelCount: Int)
    }
  }

  object Part2 {

    object ProgramActor {

      sealed trait ProgramMessage

      case class Setup(outputOpt: Option[ActorRef[DrawBotActor.Control]], lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]]) extends ProgramMessage

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
        outputToOpt: Option[ActorRef[DrawBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]],
        pointer: Int,
        inputs: Vector[BigInt],
        outputs: Vector[BigInt],
        relativeBase: Int
      ): Behavior[ProgramMessage] ={
        Behaviors.receiveMessage { case message: Input =>
          process(context, code, outputToOpt, lastResponseToOpt, pointer, inputs :+ message.value, outputs, relativeBase)
        }
      }

      def process(
        context: ActorContext[ProgramMessage],
        code: Code,
        outputToOpt: Option[ActorRef[DrawBotActor.Control]],
        lastResponseToOpt: Option[ActorRef[DrawBotActor.Control]],
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
            outputToOpt.foreach{outputTo => outputTo ! DrawBotActor.NeedInput}
            ProgramActor.askInput(context, code, outputToOpt, lastResponseToOpt, pointer, inputs, outputs, relativeBase) // request for more inputs
          case 3 =>
            val newCode = code.write(pointer + 1, relativeBase, firstParamMode, inputs.head)
            process(context, newCode, outputToOpt, lastResponseToOpt, pointer + 2, inputs.tail, outputs, relativeBase)
          case 4 =>
            val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
            outputToOpt.foreach { outputTo =>
              outputTo ! DrawBotActor.Output(outputValue)
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
            lastResponseToOpt.foreach { lastResponseTo => lastResponseTo ! DrawBotActor.Result(outputs, code, context.self) }
            Behaviors.stopped
        }
      }
    }

    sealed trait Tile

    object Empty extends Tile {
      override def toString = " "
    }

    object Wall extends Tile {
      override def toString = "#"
    }

    object Block extends Tile {
      override def toString = "O"
    }

    object Paddle extends Tile {
      override def toString = "_"
    }

    object Ball extends Tile {
      override def toString = "*"
    }

    case class Screen(zones: Vector[Vector[Tile]]) {
      val width = zones.head.size
      val height = zones.size

      override def toString() = zones.map(_.mkString).mkString("\n")

      def get(x: Int, y: Int): Tile = zones(y)(x)
      def coordOf(tile:Tile):Option[(Int,Int)] = coordsOf(tile).headOption
      def coordsOf(tile:Tile):Iterable[(Int,Int)] = {
        for {
          y <- 0 until height
          x <- 0 until width
          if get(x, y) == tile
        } yield (x, y)
      }

      def set(x: Int, y: Int, tile: Tile): Screen = Screen(zones.updated(y, zones(y).updated(x, tile)))

      def set(x: Int, y: Int, tileId: Int): Screen = {
        val tile = tileId match {
          case 0 => Empty
          case 1 => Wall
          case 2 => Block
          case 3 => Paddle
          case 4 => Ball
        }
        Screen(zones.updated(y, zones(y).updated(x, tile)))
      }

      def blocks(): Iterable[(Int, Int)] = {
        for {
          y <- 0 until height
          x <- 0 until width
          if get(x, y) == Block
        } yield (x, y)
      }

      def blocksCount(): Int = blocks.size
    }

    object Screen {
      def apply(width: Int, height: Int): Screen = {
        Screen(Vector.fill(height)(Vector.fill(width)(Empty)))
      }
    }


    object DrawBotActor {

      sealed trait Control

      case class Result(outputs: Vector[BigInt], code: Code, from: ActorRef[ProgramActor.ProgramMessage]) extends Control {
        def latestOutput = outputs.lastOption
      }

      case class Output(value: BigInt) extends Control

      def draw(programActor: ActorRef[ProgramActor.Input], listenActor: ActorRef[ListenActor.Response], screen: Screen, score:Int): Behavior[Control] = {
        val ball = screen.coordOf(Ball)
        val paddle = screen.coordOf(Paddle)
        Behaviors.receiveMessage {
          case NeedInput if ball.isDefined && paddle.isDefined =>
            val (bx,_) = screen.coordOf(Ball).get
            val (px,_) = screen.coordOf(Paddle).get
            if (px < bx) programActor ! ProgramActor.Input(1)
            else if (px > bx) programActor ! ProgramActor.Input(-1)
            else programActor ! ProgramActor.Input(0)
            draw(programActor, listenActor, screen, score)
          case Output(x) =>
            Behaviors.receiveMessage {
              case Output(y) =>
                Behaviors.receiveMessage {
                  case Output(newScore) if x == -1 =>
                    println(newScore)
                    println(screen)
                    draw(programActor, listenActor, screen, newScore.toInt)
                  case Output(tileId) =>
                    val newScreen = screen.set(x.toInt, y.toInt, tileId.toInt)
                    draw(programActor, listenActor, newScreen, score)
                }
            }
          case result: Result =>
            listenActor ! ListenActor.Response(score)
            Behaviors.stopped
        }
      }

      def apply(code: Code, listenActor: ActorRef[ListenActor.Response], startBlack: Boolean = true): Behavior[Control] = Behaviors.setup { context =>
        val programActor = context.spawn(ProgramActor(code), "program")
        programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))
        val screen = Screen(40, 40)
        draw(programActor, listenActor, screen, 0) // PART 1
      }

      object NeedInput extends Control

    }

    object ListenActor {
      case class Response(panelCount: Int)
    }

  }
}
