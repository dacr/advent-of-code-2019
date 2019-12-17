package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day17Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day17" should {
      "Part1 & 2" should {
        import Day17._
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val driverBotActor = spawn(AftScaffoldingControlActor(code, listenProbe.ref))
          val response1 = listenProbe.expectMessageType[ListenActor.Response]
          response1.value shouldBe 298 // Part1
        }
      }
    }
  }
}
