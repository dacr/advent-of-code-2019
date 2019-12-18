package adventofcode

import org.scalatest.{Matchers, WordSpec}

class Day18Test extends WordSpec with Matchers {

  "Day18" should {
    import Day18._

    "Part1" should {
      val IndexedSeq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = 'a' to 's'
      val examples = List(
        """#########
          |#b.A.@.a#
          |#########""".stripMargin
          -> (8, List(a, b)),
        """########################
          |#f.D.E.e.C.b.A.@.a.B.c.#
          |######################.#
          |#d.....................#
          |########################""".stripMargin
          -> (86, List(a, b, c, d, e, f)),
        """########################
          |#...............b.C.D.f#
          |#.######################
          |#.....@.a.B.c.d.A.e.F.g#
          |########################""".stripMargin
          -> (132, List(b, a, c, d, f, e, g)),
        """#################
          |#i.G..c...e..H.p#
          |########.########
          |#j.A..b...f..D.o#
          |########@########
          |#k.E..a...g..B.n#
          |########.########
          |#l.F..d...h..C.m#
          |#################""".stripMargin
        ->(136, List(a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m)),
        """########################
          |#@..............ac.GI.b#
          |###d#e#f################
          |###A#B#C################
          |###g#h#i################
          |########################""".stripMargin
          ->(81, List(a, c, f, i, d, g, b, e, h))
      )
      "base examples" should {
        for {((lab, (shortestPath, path)), testNumber) <- examples.zipWithIndex}
        s"example#$testNumber gives the shortest solutions" in {
          val solution = solve(lab)
          solution.shortestPathLength shouldBe shortestPath
        }
      }

      "give the right result with the provided file" in {
        val solution = solve(fileToString())
        solution.shortestPathLength shouldBe 42
      }
    }
  }
}