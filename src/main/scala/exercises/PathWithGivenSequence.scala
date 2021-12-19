package exercises

import exercises.utils.TreeDepthFirstSearch.{Branch, Tree, Empty}


object PathWithGivenSequence {

  /**
   * Given the tree and the sequence of numbers, identify if the tree has the sequence of numbers starting from the root
   * and finishing at the leaf
   *
   * @param tree     Tree structure
   * @param sequence of numbers
   * @return answer if the given sequence is present in the tree
   */
  def apply(tree: Tree, sequence: Array[Int]): Boolean = {
    val list = sequence.toList
    list match {
      case Nil => true
      case expected :: Nil => isExpectedNode(tree, expected) && isLeaf(tree)
      case expected :: next :: _ if isExpectedNode(tree, expected) => apply(getNextLeaf(tree, next), sequence.tail)
      case _ => false
    }
  }

  def isExpectedNode(tree: Tree, expectedValue: Int): Boolean = {
    tree match {
      case _@Branch(value, _, _) => value == expectedValue
      case Empty => false
    }
  }

  def getNextLeaf(tree: Tree, nextValue: Int): Tree = {
    tree match {
      case t@Branch(value, left, right) => if (nextValue > value) right else left
      case Empty => tree
    }
  }

  def isLeaf(tree: Tree): Boolean = {
    tree match {
      case _@Branch(_, Empty, Empty) => true
      case _ => false
    }
  }
}


