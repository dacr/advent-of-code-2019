package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day9Part1Test extends ScalaTestWithActorTestKit with WordSpecLike {
  "Advent of code" must {
    "Day9" should {
      "Part1" should {
        import Day9.Part1._
        "engine compute" should {
          "provide the right results on first simplified programs" in {
            val engineProbe = createTestProbe[EngineActor.Control]()
            val code = stringToCode("1102,34915192,34915192,7,4,7,99,0")
            val programActor = spawn(ProgramActor(code))
            programActor ! ProgramActor.Setup(None, engineProbe.ref)
            programActor ! ProgramActor.Input(1)
            val response = engineProbe.expectMessageType[EngineActor.Result]
            response.value shouldBe Some(BigInt("1219070632396864"))
          }
          "provide the right results on second simplified programs" in {
            val engineProbe = createTestProbe[EngineActor.Control]()
            val code = stringToCode("104,1125899906842624,99")
            val programActor = spawn(ProgramActor(code))
            programActor ! ProgramActor.Setup(None, engineProbe.ref)
            programActor ! ProgramActor.Input(1)
            val response = engineProbe.expectMessageType[EngineActor.Result]
            response.value shouldBe Some(BigInt("1125899906842624"))
          }
          "provide the right results with the provided file input" in {
            val engineProbe = createTestProbe[EngineActor.Control]()
            val code = fileToCode()
            val programActor = spawn(ProgramActor(code))
            programActor ! ProgramActor.Setup(None, engineProbe.ref)
            programActor ! ProgramActor.Input(1)
            val response = engineProbe.expectMessageType[EngineActor.Result]
            response.value shouldBe Some(BigInt("42"))
          }
        }
      }
    }
  }
}
