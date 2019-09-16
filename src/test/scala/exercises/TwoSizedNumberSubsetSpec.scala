package exercises

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv

class TwoSizedNumberSubsetSpec(implicit ee: ExecutionEnv) extends Specification {

  def is = "TwoSizedNumberSubsetSpec".title ^
    s2"""
     check TwoSizedNumberSubset  if:
    `TwoSizedNumberSubset.two_sum` must return Some with indexes for existing number pair             $e1
    `TwoSizedNumberSubset.two_sum` must return None when no pair exists                               $e2
    `BonusTwoSizedNumberSubset.two_sum` must return true when pair exists                               $e3
    `BonusTwoSizedNumberSubset.two_sum` must return false when no pair exists                               $e4
    """

  def e1 = {
    TwoSizedNumberSubset.two_sum(List(4, 7, 1, -3, 2), 5) must beSome[(Int, Int)]((2, 0))
    TwoSizedNumberSubset.two_sum(List(4, 7, 1, -3, 2), -1) must beSome[(Int, Int)]((4, 3))
  }

  def e2 = {
    TwoSizedNumberSubset.two_sum(List(4, 7, 1, -3, 2), 10) must beNone
    TwoSizedNumberSubset.two_sum(List(), 5) must beNone
    TwoSizedNumberSubset.two_sum(List(4), 4) must beNone
    TwoSizedNumberSubset.two_sum(List(4, 7, 1, -3, 2), -10) must beNone
  }

  def e3 = {
    BonusTwoSizedNumberSubset.two_sum(Array(4, 7, 1, -3, 2), 5) must beTrue
    BonusTwoSizedNumberSubset.two_sum(Array(4, 7, 1, -3, 2), -1) must beTrue
  }

  def e4 = {
    BonusTwoSizedNumberSubset.two_sum(Array(4, 7, 1, -3, 2), 10) must beFalse
    BonusTwoSizedNumberSubset.two_sum(Array(), 5) must beFalse
    BonusTwoSizedNumberSubset.two_sum(Array(4), 4) must beFalse
    BonusTwoSizedNumberSubset.two_sum(Array(4, 7, 1, -3, 2), -10) must beFalse
    BonusTwoSizedNumberSubset.two_sum(Array(1, 5), 10) must beFalse
  }
}
