package exercises

import scala.annotation.tailrec

object TwoSizedNumberSubset extends App {

  def two_sum(input: List[Int], k: Int): Option[(Int, Int)] = {
    if (input.isEmpty) {
      return None
    }

    @tailrec
    def find(checkNum: Int, checkNumIndex: Int, numbers: List[Int], valueToIdx: Map[Int, Int]): Option[(Int, Int)] = {
      valueToIdx.get(k - checkNum) match {
        case Some(idx) => Some((checkNumIndex, idx))
        case None if numbers.isEmpty => None
        case None => find(numbers.head, checkNumIndex + 1, numbers.tail, valueToIdx + (checkNum -> checkNumIndex))
      }
    }

    find(input.head, 0, input.tail, Map())
  }
}

object BonusTwoSizedNumberSubset extends App {

  def two_sum(input: Array[Int], k: Int): Boolean = {
    if (input.isEmpty) {
      return false
    }

    @tailrec
    def iterate(head: Int, tail: Array[Int]): Boolean =
      tail match {
        case Array() => false
        case nums if find(nums.tail, k - head) => true
        case nums => iterate(nums.head, nums.tail)
      }

    val sortedInput = input.sorted
    iterate(sortedInput.head, sortedInput.tail)
  }

  @tailrec
  def find(inputPart: Array[Int], expected: Int): Boolean = {
    val length = inputPart.length
    length match {
      case 0 => false
      case 1 => inputPart(0) == expected
      case _ =>
        val halfIdx = length / 2
        if (inputPart(halfIdx) > expected) {
          find(inputPart.slice(0, halfIdx), expected)
        } else {
          find(inputPart.slice(halfIdx, length), expected)
        }
    }
  }
}
