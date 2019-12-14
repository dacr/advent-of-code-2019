package adventofcode.day14

import better.files._

object Day14 {
  def fileToString(inputFile: File = "data" / "day14" / "input.txt"): String = inputFile.contentAsString

  trait Chemical {
    val name:String
  }
  object Fuel extends Chemical {
    override val name: String = "FUEL"
  }
  object Ore extends Chemical {
    override val name: String = "ORE"
  }
  case class OtherChemical(name:String) extends Chemical

  case class Ingredient(chemical: Chemical, quantity:Int)
  case class Reaction(inputs:List[Ingredient], output:Ingredient)

  case class NanoFactory(reactions:List[Reaction]) {
    val reactionForChemical =
      reactions
        .groupBy{_.output.chemical}
        .map{case (ingredient,ingredientReactions) => ingredient->ingredientReactions.head} // Always only 1 reaction for 1 output

  }

  def stringToIngredient(input:String):Ingredient = {
    input.trim.split("""\s+""") match {
      case Array(quantityString, "FUEL")=> Ingredient(Fuel, quantityString.toInt)
      case Array(quantityString, "ORE")=> Ingredient(Ore, quantityString.toInt)
      case Array(quantityString, name)=> Ingredient(new OtherChemical(name), quantityString.toInt)
    }
  }
  def stringToReaction(formula:String):Reaction = {
    formula.split("""\s*=>\s*""") match {
      case Array(inputsString, outputString)=>
        val inputs = inputsString.split("""\s*,\s*""").map(stringToIngredient)
        val output = stringToIngredient(outputString)
        Reaction(inputs.toList, output)
    }
  }

  def parseInput(chemistry:String):NanoFactory = {
    val reactions:List[Reaction] =
      chemistry
        .split("\n")
        .map(stringToReaction)
        .toList
    new NanoFactory(reactions)
  }



  object Part1 {

    def divideRoundUp(x: Int, y: Int):Int = {
      x / y + (if (x % y > 0) 1 else 0)
    }

    def howManyOre(nanoFactory: NanoFactory, chemical: Chemical, requirement:Int): Option[Int] = {
      var stocks = Map.empty[Chemical, Int]   // no stocks, remaining chemical because reaction produce too much

      def worker(goal:Chemical, requirement:Int):Option[Int] = {
        nanoFactory.reactionForChemical.get(goal).map { reaction =>
          val adjustedRequirement = stocks.get(goal) match {
            case None =>
              requirement
            case Some(stock) if stock <= requirement =>
              stocks = stocks.removed(goal)
              requirement - stock
            case Some(stock) if stock > requirement =>
              stocks = stocks + (goal -> (stock - requirement))
              0
          }
          if (adjustedRequirement == 0) 0
          else {
            val (factor, unused) = {
              if (adjustedRequirement <= reaction.output.quantity)
                (1, reaction.output.quantity - adjustedRequirement)
              else {
                val f = divideRoundUp(adjustedRequirement, reaction.output.quantity)
                (f, reaction.output.quantity * f - adjustedRequirement)
              }
            }
            if (unused > 0) stocks += goal->( stocks.get(goal).getOrElse(0) + unused)
            val (ores, notOres) = reaction.inputs.partition(_.chemical == Ore)
            ores.map(_.quantity * factor).sum + notOres.flatMap(ingredient => worker(ingredient.chemical, factor * ingredient.quantity)).sum
          }
        }
      }
      worker(chemical, requirement)
    }

    def bestSolution(chemistry:String):Option[Int] = {
      val nanoFactory = parseInput(chemistry)
      howManyOre(nanoFactory, Fuel, 1)
    }
  }



  object Part2 {

    def divideRoundUp(x: Long, y: Long):Long = {
      x / y + (if (x % y > 0L) 1L else 0L)
    }


    var stocks = collection.mutable.Map.empty[Chemical, Long]   // no stocks, remaining chemical because reaction produce too much

    def howManyOre(nanoFactory: NanoFactory, chemical: Chemical, requirement:Long): Option[Long] = {
      def worker(goal:Chemical, requirement:Long):Option[Long] = {
        nanoFactory.reactionForChemical.get(goal).map { reaction =>
          val adjustedRequirement = stocks.get(goal) match {
            case None =>
              requirement
            case Some(stock) if stock <= requirement =>
              stocks(goal) = 0
              requirement - stock
            case Some(stock) if stock > requirement =>
              stocks(goal) = (stock - requirement)
              0
          }
          if (adjustedRequirement == 0L) 0L
          else {
            val (factor, unused) = {
              if (adjustedRequirement <= reaction.output.quantity)
                (1L, reaction.output.quantity - adjustedRequirement)
              else {
                val f = divideRoundUp(adjustedRequirement, reaction.output.quantity)
                (f, reaction.output.quantity * f - adjustedRequirement)
              }
            }
            if (unused > 0L) stocks(goal) =  stocks.get(goal).getOrElse(0L) + unused
            val (ores, notOres) = reaction.inputs.partition(_.chemical == Ore)
            ores.map(_.quantity.toLong * factor).sum + notOres.flatMap(ingredient => worker(ingredient.chemical, factor * ingredient.quantity.toLong)).sum
          }
        }
      }
      val result = worker(chemical, requirement)
      //println(stocks)
      result
    }

    def bestSolution(chemistry:String):Long = {
      val nanoFactory = parseInput(chemistry)
      val oresGoal = 1000000000000L
      var remaining = oresGoal
      var fuelProduced = 0L
      while(remaining > 0L) {
        if (fuelProduced % 100_000L == 0L) println(remaining)
        val requiredOres = howManyOre(nanoFactory, Fuel, 1L).getOrElse(0L)
        remaining -= requiredOres
        if (remaining > 0) fuelProduced += 1
      }
      fuelProduced
    }



  }

  def main(args: Array[String]): Unit = {
    import Part2._
    bestSolution(fileToString())
  }
}
