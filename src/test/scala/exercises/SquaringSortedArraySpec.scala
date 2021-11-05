package exercises

import org.specs2.Specification

class SquaringSortedArraySpec extends Specification {

  def is = "SquaringSortedArray".title ^
    s2"""
     check FruitsIntoBasketsSpec if:
    `SquaringSortedArray` should pass example 1          $e1
    `FruitsIntoBaskets`   should pass example 2            $e2
    `FruitsIntoBaskets`   should pass example 3            $e3
    """

  def e1 = {
    SquaringSortedArray(Array(-1, 0)) must beEqualTo(Array(0, 1))
  }
  def e2 = {
    SquaringSortedArray(Array(-2, -1, 0, 2, 3)) must beEqualTo(Array(0, 1, 4, 4, 9))
  }
  def e3 = {
    SquaringSortedArray(Array(-3, -1, 0, 1, 2)) must beEqualTo(Array(0, 1, 1, 4, 9))
  }
}
