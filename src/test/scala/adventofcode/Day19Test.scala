package adventofcode

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.WordSpecLike
import org.scalatest.OptionValues._

import akka.testkit.TestDuration
import scala.concurrent.duration._

class Day19Test extends ScalaTestWithActorTestKit with WordSpecLike  {

  "Advent of code" must {
    "Day19" should {
      import Day19._
      "work with the provided input file" in {
        val listenProbe = createTestProbe[ListenActor.Response]()
        val code = fileToCode()
        val driverBotActor = spawn(BeamControlActor(code, listenProbe.ref, areaLimit = (50, 50)))
        listenProbe.within(10.seconds) {
          val response1 = listenProbe.expectMessageType[ListenActor.Response]
          response1.value shouldBe 121 // Part1
        }
      }
    }
  }
}