package adventofcode

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import better.files._


object Day11 {


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
    case class Setup(outputOpt:Option[ActorRef[DriveBotActor.Control]], lastResponseToOpt:Option[ActorRef[DriveBotActor.Control]]) extends ProgramMessage
    case class Input(value:BigInt) extends ProgramMessage

    def apply(code: Code):Behavior[ProgramMessage] = Behaviors.setup { context => {
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
      context:ActorContext[ProgramMessage],
      code: Code,
      outputToOpt:Option[ActorRef[DriveBotActor.Control]],
      lastResponseToOpt:Option[ActorRef[DriveBotActor.Control]],
      pointer:Int,
      inputs:Vector[BigInt],
      outputs:Vector[BigInt],
      relativeBase:Int
    ): Behavior[ProgramMessage] =
      Behaviors.receiveMessage { case message:Input =>
        process(context, code, outputToOpt, lastResponseToOpt, pointer, inputs :+ message.value, outputs, relativeBase)
      }

    def process(
      context:ActorContext[ProgramMessage],
      code: Code,
      outputToOpt:Option[ActorRef[DriveBotActor.Control]],
      lastResponseToOpt:Option[ActorRef[DriveBotActor.Control]],
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
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs,relativeBase)
        case 2 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, firstParam * secondParam)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs,relativeBase)
        case 3 if inputs.isEmpty =>
          ProgramActor.askInput(context, code, outputToOpt, lastResponseToOpt, pointer, inputs, outputs,relativeBase) // request for more inputs
        case 3 =>
          val newCode = code.write(pointer+1, relativeBase, firstParamMode, inputs.head)
          process(context, newCode, outputToOpt, lastResponseToOpt, pointer + 2, inputs.tail, outputs,relativeBase)
        case 4 =>
          val outputValue = code.read(pointer + 1, relativeBase, firstParamMode)
          outputToOpt.foreach { outputTo =>
            outputTo ! DriveBotActor.Output(outputValue)
          }
          process(context, code, outputToOpt, lastResponseToOpt, pointer + 2, inputs, outputs:+outputValue,relativeBase)
        case 5 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          if (firstParam != 0)
            process(context, code, outputToOpt, lastResponseToOpt, secondParam.toInt, inputs, outputs,relativeBase)
          else
            process(context, code, outputToOpt, lastResponseToOpt, pointer + 3, inputs, outputs,relativeBase)
        case 6 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          if (firstParam == 0)
            process(context, code, outputToOpt, lastResponseToOpt, secondParam.toInt, inputs, outputs,relativeBase)
          else
            process(context, code, outputToOpt, lastResponseToOpt, pointer + 3, inputs, outputs,relativeBase)
        case 7 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam < secondParam) 1 else 0)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs,relativeBase)
        case 8 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val secondParam = code.read(pointer + 2, relativeBase, secondParamMode)
          val newProgram = code.write(pointer + 3, relativeBase, thirdParamMode, if (firstParam == secondParam) 1 else 0)
          process(context, newProgram, outputToOpt, lastResponseToOpt, pointer + 4, inputs, outputs,relativeBase)
        case 9 =>
          val firstParam = code.read(pointer + 1, relativeBase, firstParamMode)
          val newRelativeBase = relativeBase + firstParam.toInt
          process(context, code, outputToOpt, lastResponseToOpt, pointer + 2, inputs, outputs, newRelativeBase)
        case 99 =>
          lastResponseToOpt.foreach{lastResponseTo => lastResponseTo ! DriveBotActor.Result(outputs, code, context.self)}
          Behaviors.stopped
      }
    }
  }

  sealed trait Direction
  object Up extends Direction {override def toString="Up"}
  object Down extends Direction {override def toString="Down"}
  object Left extends Direction {override def toString="Left"}
  object Right extends Direction {override def toString="Right"}

  case class Area(zones:Vector[Vector[Char]]) {
    val width = zones.head.size
    val height = zones.size
    override def toString() = zones.map(_.mkString).mkString("\n")
    def get(x: Int, y: Int): Char = zones(y)(x)

    def paint(x:Int, y:Int, color:Char):Area = Area(zones.updated(y, zones(y).updated(x, color)))
    def paintWhite(x: Int, y:Int): Area = paint(x,y,'#')
    def paintBlack(x: Int, y:Int): Area = paint(x,y,'.')
    def painted():Iterable[(BigDecimal,BigDecimal)] = {
      for {
        y <- 0 until height
        x <- 0 until width
        if get(x,y) == '#'
      } yield (x,y)
    }
    def paintedCount():Int = painted.size
  }
  object Area {
    def apply(width:Int, height:Int):Area = {
      Area(Vector.fill(height)(Vector.fill(width)('.')))
    }
  }


  object DriveBotActor {
    sealed trait Control
    case class Result(outputs:Vector[BigInt], code:Code, from:ActorRef[ProgramActor.ProgramMessage]) extends Control {
      def latestOutput = outputs.lastOption
    }
    case class Output(value:BigInt) extends Control

    def turnLeft(direction: Direction): Direction = direction match {
      case Up => Left
      case Left => Down
      case Down => Right
      case Right => Up
    }
    def turnRight(direction: Direction): Direction = direction match {
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up
    }

    def move(position: (Int, Int), direction: Direction): (Int, Int) = direction match {
      case Up => position match {case (x,y) => (x,y-1)}
      case Down =>position match {case (x,y) => (x,y+1)}
      case Left =>position match {case (x,y) => (x-1,y)}
      case Right => position match {case (x,y) => (x+1,y)}
    }

    def drive(programActor:ActorRef[ProgramActor.Input], listenActor:ActorRef[ListenActor.Response], area:Area, direction:Direction, position: (Int, Int)):Behavior[Control] = {
      position match {
        case (x,y) if area.get(x,y) == '.' =>
          programActor ! ProgramActor.Input(0)
        case (x,y) if area.get(x,y) == '#' =>
          programActor ! ProgramActor.Input(1)
      }
      Behaviors.receiveMessage{
        case Output(color) =>
          val newArea = position match {
            case (x,y) if color == 0 => area.paintBlack(x,y)
            case (x,y) if color == 1 => area.paintWhite(x,y)
          }
          Behaviors.receiveMessage {
            case Output(turnInstruction) if turnInstruction == 0 =>
              val newDirection = turnLeft(direction)
              val newPosition = move(position, newDirection)
              println(s"paint $color, turnLeft $direction->$newDirection and move to $position->$newPosition")
              println(newArea)
              drive(programActor, listenActor, newArea, newDirection, newPosition)
            case Output(turnInstruction) if turnInstruction == 1 =>
              val newDirection = turnRight(direction)
              val newPosition = move(position, newDirection)
              println(s"paint $color, turnRight $direction->$newDirection and move to $position->$newPosition")
              println(newArea)
              drive(programActor, listenActor, newArea, newDirection, newPosition)
          }
        case result:Result =>
          println(s"STOPPED\n$area")
          listenActor ! ListenActor.Response(area.paintedCount())
          Behaviors.stopped
      }
    }

    def apply(code:Code, listenActor:ActorRef[ListenActor.Response]):Behavior[Control] = Behaviors.setup{ context =>
      val programActor = context.spawn(ProgramActor(code), "program")
      programActor ! ProgramActor.Setup(Some(context.self), Some(context.self))
      //val area = Area(160,140)
      //val area = Area(500,500)
      val area = Area(4,4)
      drive(programActor, listenActor, area, Up, (area.width / 2, area.height / 2))
    }
  }

  object ListenActor {
    case class Response(panelCount:Int)
  }

  def stringToCode(program:String):Code = new Code(program.split(",").toVector.map(x => BigInt(x)))

  def fileToCode(inputFile: File = "data" / "day11" / "input.txt"): Code = {
    stringToCode(inputFile.contentAsString)
  }

}
