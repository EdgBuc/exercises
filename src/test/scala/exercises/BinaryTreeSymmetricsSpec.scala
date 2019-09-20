package exercises

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv

class BinaryTreeSymmetricsSpec(implicit ee: ExecutionEnv) extends Specification {

  def is = "BinaryTreeSymmetrics".title ^
    s2"""
     check BinaryTreeSymmetrics if:
    `BinaryTreeSymmetrics.isSymmetricArrayIterable` should tell if array based tree symmetric             $e1
    `BinaryTreeSymmetrics.isSymmetricArrayRecursive` should tell if array based tree symmetric             $e2
    """

  def e1 = {
    BinaryTreeSymmetrics.isSymmetricArrayIterable(Array(1, 2, 2, 3, 4, 4, 3)) must beTrue
    BinaryTreeSymmetrics.isSymmetricArrayIterable(Array(1, 2, 2, 3, 4, 3, 4)) must beFalse
  }

  def e2 = {
    BinaryTreeSymmetrics.isSymmetricArrayRecursive(Array(1, 2, 2, 3, 4, 4, 3)) must beTrue
    BinaryTreeSymmetrics.isSymmetricArrayRecursive(Array(1, 2, 2, 3, 4, 3, 4)) must beFalse
  }
}
