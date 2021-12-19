package exercises.utils

object TreeDepthFirstSearch {
  sealed trait Tree
  case class Branch(value: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  object Tree {
    val empty: Tree = Empty

    def add(tree: Tree, x: Int): Tree = tree match {
      case t @ Branch(value, left, right) =>
        if (x < value) {
          t.copy(left = add(left, x))
        } else if (x > value) {
          t.copy(right = add(right, x))
        } else {
          t
        }
      case Empty => Branch(x, Empty, Empty)
    }

    def apply(xs: Int*): Tree =
      xs.foldLeft(empty)(add)
  }
}
