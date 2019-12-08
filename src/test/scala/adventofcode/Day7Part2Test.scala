package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.{Matchers, WordSpec, WordSpecLike}

class Day7Part2Test extends ScalaTestWithActorTestKit with WordSpecLike {
  "Advent of code" must {
    "Day7" should {
      "Part2" should {
        import Day7.Part2._
        "engine compute" should {
          "provide the right results on first simplified programs" in {
            val probe = createTestProbe[SolverActor.BestResult]()
            val code = stringToCode("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
            val solverActor = spawn(SolverActor())
            solverActor ! SolverActor.Start(code, 0, probe.ref)
            val response = probe.expectMessageType[SolverActor.BestResult]
            response.value shouldBe 139629729
          }
          "provide the right results on second simplified programs" in {
            val probe = createTestProbe[SolverActor.BestResult]()
            val code = stringToCode("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
            val solverActor = spawn(SolverActor())
            solverActor ! SolverActor.Start(code, 0, probe.ref)
            val response = probe.expectMessageType[SolverActor.BestResult]
            response.value shouldBe 18216
          }
          "provide the right solution with the provided file" in {
            val probe = createTestProbe[SolverActor.BestResult]()
            val code = fileToCode()
            val solverActor = spawn(SolverActor())
            solverActor ! SolverActor.Start(code, 0, probe.ref)
            val response = probe.expectMessageType[SolverActor.BestResult]
            response.value shouldBe 8754464
          }
        }
      }
    }
  }
}
