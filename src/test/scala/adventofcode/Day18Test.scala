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
          |#@..............ac.GI.b#
          |###d#e#f################
          |###A#B#C################
          |###g#h#i################
          |########################""".stripMargin
          ->(81, List(a, c, f, i, d, g, b, e, h)),
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
      )

      "land parser" should {
        for {((lab, (_, _)), testNumber) <- examples.zipWithIndex }
          s"parse example#$testNumber" in {
            Land(lab).toString() shouldBe lab
          }
      }

      "choices function" should {
        "work on sample1" in {
          val sample =
            """#########
              |#b.A.@.a#
              |#########""".stripMargin
          val lab = Land(sample)
          searchChoices(lab.startPosition().get, lab) should contain allOf(
            Choice(ItemPos(Item('A'),(3,1)), 2),
            Choice(ItemPos(Item('a'),(7,1)), 2),
          )
        }
        "work on sample2" in {
          val sample =
            """############
              |#b.A...@..a#
              |############""".stripMargin
          val lab = Land(sample)
          searchChoices(lab.startPosition().get, lab) should contain allOf(
            Choice(ItemPos(Item('A'),(3,1)), 4),
            Choice(ItemPos(Item('a'),(10,1)), 3),
          )
        }
      }

      "SimpleTest" in {
        val sample =
          """############
            |#b.A...@..a#
            |############""".stripMargin
        val solution = solve(sample).minBy(_.shortestPathLength)
        solution.shortestPathLength shouldBe 12
        solution.collectedKeys.map(_.name) shouldBe List('a', 'b')
      }


      "base examples" should {
        for {((lab, (shortestPath, path)), testNumber) <- examples.zipWithIndex}
        s"example#$testNumber gives the shortest solutions" in {
          solve(lab).groupBy(_.shortestPathLength).toList.minBy{case(len,_)=> len} match {
            case (len,solutions)=>
              println("-------------------------")
              println(s"Test#$testNumber $len")
              solutions.foreach{println}
              len shouldBe shortestPath
              solutions.map(_.collectedKeys.map(_.name)) should contain oneElementOf(List(path))
          }
        }
      }

      "give the right result with the provided file" in {
        val solution = solve(fileToString()).minBy(_.shortestPathLength)
        solution.shortestPathLength shouldBe 42
      }
    }
  }
}