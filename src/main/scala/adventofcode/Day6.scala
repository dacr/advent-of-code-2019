package adventofcode

import better.files._

import scala.annotation.tailrec


object Day6 {

  object Part1 {

    case class Node(name: String)

    case class Tree(node: Node, children: List[Tree])


    def buildTree(treeDesc: String): Tree = {
      val connections =
        treeDesc
          .split("\n")
          .map(_.split("[)]", 2))
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
      assert(rootNodes.size == 1)
      build(rootNodes.head, connections)
    }

    def treeDepthsSum(tree: Tree): Int = {
      def walk(tree: Tree, depth: Int = 0): Int = tree.children match {
        case Nil => depth
        case subTrees => depth + subTrees.map(subTree => walk(subTree, depth + 1)).sum
      }

      walk(tree)
    }


    def executeWithInputString(input: String): Int = {
      val tree: Tree = buildTree(input)
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
  }

}
