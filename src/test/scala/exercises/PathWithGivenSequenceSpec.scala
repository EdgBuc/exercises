package exercises

import exercises.utils.TreeDepthFirstSearch.Tree
import org.specs2.mutable.SpecificationWithJUnit

class PathWithGivenSequenceSpec extends SpecificationWithJUnit {
  "PathWithGivenSequence" should {
    "pass example 0" in {
      val tree: Tree = Tree()

      PathWithGivenSequence(tree, Array()) must beTrue
    }

    "pass example 1" in {
      val tree: Tree = Tree(1)

      PathWithGivenSequence(tree, Array(1)) must beTrue
    }

    "pass example 2" in {
      val tree: Tree = Tree(1, 2)

      PathWithGivenSequence(tree, Array(1, 2)) must beTrue
    }

    "pass example 3" in {
      val tree: Tree = Tree(2, 1, 3)

      PathWithGivenSequence(tree, Array(2, 3)) must beTrue
    }

    "pass example 4" in {
      val tree: Tree = Tree(2, 1, 3)

      PathWithGivenSequence(tree, Array(1, 3)) must beFalse
    }

    "pass example 5" in {
      val tree: Tree = Tree(1, 2, 3)

      PathWithGivenSequence(tree, Array(2, 3)) must beFalse
    }

    "pass example 6" in {
      val tree: Tree = Tree(1, 1, 1, 2, 2, 3, 2)

      PathWithGivenSequence(tree, Array(1, 2, 3)) must beTrue
    }

    "pass example 7" in {
      val tree: Tree = Tree(1, 2, 3, 4)

      PathWithGivenSequence(tree, Array(1, 2, 3)) must beFalse
    }

    "pass example 8" in {
      val tree: Tree = Tree(1, 2)

      PathWithGivenSequence(tree, Array(1, 2, 3)) must beFalse
    }
  }
}
