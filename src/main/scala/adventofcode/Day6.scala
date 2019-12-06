package adventofcode

import better.files._

object Day6 {

  case class Node(name: String)

  case class Tree(node: Node, children: List[Tree])

  def buildTree(treeDesc: String, connectionMarkerRegex:String="[)]"): List[Tree] = {
    val connections =
      treeDesc
        .split("\n")
        .map(_.split(connectionMarkerRegex, 2))
        .map { case Array(aName, bName) => Node(aName) -> Node(bName) }
        .toList
        .groupMap { case (a, _) => a } { case (_, b) => b }
    val children = connections.values.flatten.toList
    val nodes = connections.keys.toSet ++ children
    val rootNodes = nodes -- children
    def build(fromNode: Node, remainingConnections: Map[Node, List[Node]]): Tree = {
      val newRemainingConnections = remainingConnections - fromNode
      val subtrees =
        remainingConnections
          .get(fromNode)
          .getOrElse(Nil)
          .map(node => build(node,newRemainingConnections))
      Tree(fromNode, subtrees)
    }
    rootNodes.toList.map(rootNode => build(rootNode, connections))
  }


  object Part1 {

    def treeDepthsSum(tree: Tree): Int = {
      def walk(tree: Tree, depth: Int = 0): Int = tree.children match {
        case Nil => depth
        case subTrees => depth + subTrees.map(subTree => walk(subTree, depth + 1)).sum
      }
      walk(tree)
    }

    def executeWithInputString(input: String): Int = {
      val tree: Tree = buildTree(input).head
      treeDepthsSum(tree)
    }

    def executeWithInputFile(): Int = {
      val inputFile = "data" / "day6" / "part1" / "input.txt"
      val input = inputFile.contentAsString
      executeWithInputString(input)
    }
  }

  // ========================================================================================

  object Part2 {

    def distanceTo(tree:Tree, node:Node):Option[Int] = {
      def searchDistance(current:Tree, node:Node, depth:Int):Option[Int] = {
        if (current.node == node) Some(depth)
        else if (current.children.isEmpty) None
        else {
          current
            .children
            .to(LazyList)
            .map(subTree => searchDistance(subTree, node, depth+1))
            .find(_.isDefined)
            .flatten
        }
      }
      searchDistance(tree, node, 0)
    }

    def distanceBetween(tree: Tree, nodeA: Node, nodeB: Node): Option[Int] = {
      def explore(currentTree:Tree, bestDistance:Option[Int]):Option[Int] = {
        (distanceTo(currentTree, nodeA), distanceTo(currentTree, nodeB)) match {
          case (None, _) => bestDistance
          case (_, None) => bestDistance
          case (Some(da), Some(db)) =>
            val newBestDistance = bestDistance.filter(_ < (da+db)).orElse(Some(da+db))
            val results = currentTree.children.map(child => explore(child, newBestDistance)).flatten
            results.minByOption(x => x).orElse(newBestDistance)
        }
      }
      explore(tree, None)
    }

    def executeWithInputString(input: String): Int = {
      val tree: Tree = buildTree(input).head
      distanceBetween(tree, Node("SAN"), Node("YOU")).map(_ - 2).getOrElse(-1)
    }

    def executeWithInputFile(): Int = {
      val inputFile = "data" / "day6" / "part1" / "input.txt"
      val input = inputFile.contentAsString
      executeWithInputString(input)
    }
  }


}
