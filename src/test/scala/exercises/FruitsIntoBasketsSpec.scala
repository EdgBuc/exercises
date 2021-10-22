package exercises

import org.specs2.Specification

class FruitsIntoBasketsSpec extends Specification {

  def is = "FruitsIntoBasketsSpec".title ^
    s2"""
     check FruitsIntoBasketsSpec if:
    `FruitsIntoBaskets` should calculate correctly fruits [A A A B]           $e1
    `FruitsIntoBaskets` should calculate correctly fruits [D E E E]            $e2
    `FruitsIntoBaskets` should calculate correctly fruits [B C B B C]            $e3
    `FruitsIntoBaskets` should calculate correctly fruits [A A B A A B]            $e4
    `FruitsIntoBaskets` should calculate correctly fruits [A]           $e5
    `FruitsIntoBaskets` should calculate correctly fruits [A B]           $e6
    `FruitsIntoBaskets` should calculate correctly fruits [A A]            $e7
    `FruitsIntoBaskets` should calculate correctly fruits EMPTY            $e8
    `FruitsIntoBaskets` should calculate correctly fruits [C C D D C]            $e9
    """

  def e1 = {
    FruitsIntoBaskets(Array('A', 'A', 'A', 'B', 'C')) must beEqualTo(4)
  }
  def e2 = {
    FruitsIntoBaskets(Array('A', 'B', 'C', 'A', 'C', 'D', 'E', 'E', 'E')) must beEqualTo(4)
  }
  def e3 = {
    FruitsIntoBaskets(Array('A', 'B', 'C', 'B', 'B', 'C')) must beEqualTo(5)
  }
  def e4 = {
    FruitsIntoBaskets(Array('A', 'A', 'B', 'A', 'A', 'B', 'C', 'B', 'B', 'C')) must beEqualTo(6)
  }
  def e5 = {
    FruitsIntoBaskets(Array('A')) must beEqualTo(1)
  }
  def e6 = {
    FruitsIntoBaskets(Array('A', 'B', 'C')) must beEqualTo(2)
  }
  def e7 = {
    FruitsIntoBaskets(Array('A', 'A')) must beEqualTo(2)
  }
  def e8 = {
    FruitsIntoBaskets(Array()) must beEqualTo(0)
  }
  def e9 = {
    FruitsIntoBaskets(Array('A', 'B', 'C', 'C', 'D', 'D', 'C')) must beEqualTo(5)
  }
}
