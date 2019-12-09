package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.{Assertion, WordSpecLike}
import better.files._

class Day9Test extends ScalaTestWithActorTestKit with WordSpecLike {
  "Advent of code" must {
    "Day9" should {
      import Day9._
      "engine compute" should {

        def testProgramCode(program:String, expectedCodeProgram:String, input:BigInt): Assertion = {
          val engineProbe = createTestProbe[EngineActor.Control]()
          val code = stringToCode(program)
          val programActor = spawn(ProgramActor(code))
          programActor ! ProgramActor.Setup(None, engineProbe.ref)
          programActor ! ProgramActor.Input(input)
          val response = engineProbe.expectMessageType[EngineActor.Result]
          response.code.program.mkString(",") shouldBe expectedCodeProgram
        }

        def testProgram(program:String, expectedResult:String, input:BigInt): Assertion = {
          val engineProbe = createTestProbe[EngineActor.Control]()
          val code = stringToCode(program)
          val programActor = spawn(ProgramActor(code))
          programActor ! ProgramActor.Setup(None, engineProbe.ref)
          programActor ! ProgramActor.Input(input)
          val response = engineProbe.expectMessageType[EngineActor.Result]
          response.outputs.mkString(",") shouldBe expectedResult
        }

        def testProgramFile(programFile:File, expectedResult:String, input:BigInt):Assertion =
          testProgram(programFile.contentAsString, expectedResult, input)

        for{
          ((inputProgram, inputValue, expectedProgram), testNum) <- List(
            ("1002,4,3,4,33", "1002,4,3,4,99", 1),
            ("11102,1,2,0,99", "11102,1,2,2,99", 1),
            ("1101,100,-1,4,0", "1101,100,-1,4,99", 1),
            ("203,3,99", "203,3,99,1", 1),
            ("9,1,203,5,99", "9,1,203,5,99,0,42", 42),
            ("109,2,203,5,99", "109,2,203,5,99,0,0,42", 42),
            ("109,8,203,-1,99", "109,8,203,-1,99,0,0,42", 42),
            ("21101,42,24,5,99", "21101,42,24,5,99,66", 1),
            ("109,0,21101,42,24,7,99", "109,0,21101,42,24,7,99,66", 1),
            ("209,1,21101,42,24,6,99", "209,1,21101,42,24,6,99,66", 1),
          ).zipWithIndex
        } {
          s"test configuration number#$testNum" in {
            testProgramCode(inputProgram, inputValue, expectedProgram)
          }
        }


        val conditionsTestProgram =
          """3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            |1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            |999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99""".stripMargin.replaceAll("\n", "")

        for( ((expectedValue, inputValue), testNum) <- List( ("999", 5), ("1000", 8), ("1001", 42) ).zipWithIndex) {
          s"test conditions configuration number#$testNum" in {
            testProgram(conditionsTestProgram, expectedValue, inputValue)
          }
        }


        "is quinable" in { // https://en.wikipedia.org/wiki/Quine_(computing)
          testProgram(
            "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
            "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99", 1
          )
        }

        "provide the right results on first simplified programs" in {
          testProgram(
            "1102,34915192,34915192,7,4,7,99,0",
            "1219070632396864", 1
          )
        }
        "provide the right results on second simplified programs" in {
          testProgram(
            "104,1125899906842624,99",
            "1125899906842624", 1
          )
        }

        "provide the right results with the day5 part1 provided file input" in {
          testProgramFile(
            "data" / "day5" / "part1" / "input.txt",
            "0,0,0,0,0,0,0,0,0,9219874", 1
          )
        }

        "provide the right results with the day5 part2 provided file input" in {
          testProgramFile(
            "data" / "day5" / "part1" / "input.txt",
            "5893654",
            5
          )
        }

        "provide the right results with the provided file input" in {
          testProgramFile(
            "data" / "day9" / "part1" / "input.txt",
            "2594708277", 1
          )

        }

        "provide the right results with the provided file input (part2)" in {
          testProgramFile(
            "data" / "day9" / "part1" / "input.txt",
            "87721", 2
          )
        }

      }
    }
  }
}
