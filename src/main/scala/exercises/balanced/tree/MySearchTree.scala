package exercises.balanced.tree

import exercises.balanced.tree.MySearchTree.validate

import scala.annotation.tailrec


class EmptySearchTree extends MySearchTree(0) {
  override def contains(expectedValue: Int): Boolean = false

  override def withValue(expectedValue: Int): MySearchTree = this
}

case class MySearchTree(
                         value: Int,
                         var left: Option[MySearchTree] = None,
                         var right: Option[MySearchTree] = None,
                         var isRed: Boolean = true,
                         var parent: Option[MySearchTree] = None) extends LookupIndex[Int] {

  override def contains(expectedValue: Int): Boolean =
    find(Some(this), expectedValue) match {
      case None => false
      case _ => true
    }

  override def withValue(expectedValue: Int): MySearchTree =
    find(Some(this), expectedValue).getOrElse(new EmptySearchTree)

  @tailrec
  private def find(treeOpt: Option[MySearchTree], expectedValue: Int): Option[MySearchTree] = {
    treeOpt match {
      case Some(tree) if (tree.value == expectedValue) => treeOpt
      case Some(MySearchTree(value, left, right, _, _)) => if (value > expectedValue) find(left, expectedValue) else find(right, expectedValue)
      case _ => None
    }
  }

  def addChild(node: MySearchTree): Unit = {
    if (this.value > node.value) {
      this.left = Some(node)
    } else {
      this.right = Some(node)
    }
    node.parent = Some(this)
    validate(node)
  }

  override def toString: String = s"($value, ${left.getOrElse("-")}, ${right.getOrElse("-")}, ${if (isRed) "RED" else "BLACK"})"
}

object MySearchTree {

  @tailrec
  def findParent(treeOpt: MySearchTree, x: Int): MySearchTree = {
    treeOpt match {
      case node@MySearchTree(value, None, _, _, _) if (value > x) => node
      case MySearchTree(value, Some(left), _, _, _) if (value > x) => findParent(left, x)
      case node@MySearchTree(value, _, None, _, _) if (value <= x) => node
      case MySearchTree(value, _, Some(right), _, _) if (value <= x) => findParent(right, x)
    }
  }

  def fromInts(values: Int*): MySearchTree = {
    if (values.isEmpty) {
      return new EmptySearchTree
    }

    values.tail.foldLeft(MySearchTree(value = values.head, isRed = false))((acc, x) => {
      findParent(acc, x).addChild(MySearchTree(x))
      root(acc)
    })
  }

  @tailrec
  def root(newNode: MySearchTree): MySearchTree = {
    newNode match {
      case MySearchTree(_, _, _, _, Some(parent)) => root(parent)
      case _ => newNode
    }
  }

  def validate(node: MySearchTree): Unit = {
    node.parent match {
      case None => node.isRed = false
      case _ => whenParentIsBlack(node)
    }
  }

  def whenParentIsBlack(node: MySearchTree): Unit = {
    node.parent match {
      case Some(MySearchTree(_, _, _, false, _)) =>
      case _ => whenParentAndUncleRed(node)
    }
  }

  def whenParentAndUncleRed(node: MySearchTree): Unit = {
    val parent = node.parent.get

    uncle(parent) match {
      case Some(u@MySearchTree(_, _, _, true, _)) =>
        parent.isRed = false
        u.isRed = false
        val grandparent = parent.parent.get
        grandparent.isRed = true
        validate(grandparent)
      case _ => whenInnerRedLeaf(node)
    }
  }

  def whenInnerRedLeaf(node: MySearchTree): Unit = {
    val parent = node.parent.get
    val grandParent = parent.parent.get
    if ((parent.right.contains(node)) && grandParent.left.contains(parent)) {
      rotateLeft(parent)
      whenOuterLeafOfReds(node.left.get)
    } else if (parent.left.contains(node) && grandParent.right.contains(parent)) {
      rotateRight(parent)
      whenOuterLeafOfReds(node.right.get)
    } else {
      whenOuterLeafOfReds(node)
    }
  }

  def whenOuterLeafOfReds(node: MySearchTree): Unit = {
    val parent = node.parent.get
    val grandParent = parent.parent.get
    parent.isRed = false
    grandParent.isRed = true
    if (parent.left.contains(node) && grandParent.left.contains(parent)) {
      rotateRight(grandParent)
    } else {
      rotateLeft(grandParent)
    }
  }

  def rotateLeft(n: MySearchTree): Unit = {
    val newNode = n.right.get
    moveToOppositeSide(n, newNode)
    n.right = newNode.left
    newNode.left match {
      case Some(left) => left.parent = Some(n)
      case _ => //do nothing
    }
    n.parent = Some(newNode)
    newNode.left = Some(n)
  }

  def rotateRight(n: MySearchTree): Unit = {
    val newNode = n.left.get
    moveToOppositeSide(n, newNode)
    n.left = newNode.right
    newNode.right match {
      case Some(right) => right.parent = Some(n)
      case _ => //do nothing
    }
    n.parent = Some(newNode)
    newNode.right = Some(n)
  }

  private def moveToOppositeSide(n: MySearchTree, newNode: MySearchTree): Unit = {
    newNode.parent = n.parent
    n.parent match {
      case Some(parent) =>
        if (parent.left.contains(n)) {
          parent.left = Some(newNode)
        } else {
          parent.right = Some(newNode)
        }
      case _ => //do nothing
    }
  }

  private def uncle(parent: MySearchTree): Option[MySearchTree] = {
    parent.parent match {
      case None => None
      case Some(node) =>
        if (node.left == parent.parent) {
          node.right
        } else {
          node.left
        }
    }
  }
}
