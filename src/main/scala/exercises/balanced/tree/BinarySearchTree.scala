package exercises.balanced.tree

import scala.Ordering.Implicits._

sealed trait BinarySearchTree[V] extends LookupIndex[V] {
  def withValue(value: V): BinarySearchTree[V]
}

object BinarySearchTree {
  private type Tree[V] = BinarySearchTree[V]

  def apply[V : Ordering](): Tree[V] =
    Empty()

  def fromValues[V : Ordering](values: V*): Tree[V] =
    values.foldLeft(apply[V]()) { case (acc, v) => acc.withValue(v) }

  def fromInts(values: Int*): Tree[Int] =
    fromValues(values: _*)

  private case class Empty[V : Ordering]() extends Tree[V] {
    override def contains(value: V): Boolean = false

    override def withValue(value: V): Tree[V] = Node(value, this, this)
  }

  private case class Node[V : Ordering](value: V, left: Tree[V], right: Tree[V]) extends Tree[V] {
    override def contains(other: V): Boolean = {
      if (value > other) left.contains(other)
      else if (value < other) right.contains(other)
      else true
    }

    override def withValue(other: V): Tree[V] = {
      if (value > other) copy(left = left.withValue(other))
      else if (value < other) copy(right = right.withValue(other))
      else this
    }
  }
}
