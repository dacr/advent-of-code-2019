package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day15Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day15" should {
      "Part1 & 2" should {
        import Day15._
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val driverBotActor = spawn(SearchBotActor(code, listenProbe.ref))
          val response1 = listenProbe.expectMessageType[ListenActor.Response]
          response1.value shouldBe 298 // Part1
          val response2 = listenProbe.expectMessageType[ListenActor.Response]
          response2.value shouldBe 346 // Part2
        }
      }
    }
  }
}
