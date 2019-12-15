package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike

class Day15Test extends ScalaTestWithActorTestKit with WordSpecLike {

  "Advent of code" must {
    "Day15" should {
      "Part1" should {
        import Day15.Part1._
        import Day15._
        "work with the provided input file" in {
          val listenProbe = createTestProbe[ListenActor.Response]()
          val code = fileToCode()
          val DriverBotActor = spawn(SearchBotActor(code, listenProbe.ref))
          val response = listenProbe.expectMessageType[ListenActor.Response]
          response.distance shouldBe 298
        }
      }
    }
  }
}
