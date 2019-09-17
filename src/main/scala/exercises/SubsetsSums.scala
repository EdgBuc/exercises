package exercises

import scala.annotation.tailrec

object SubsetsSums extends App {

  def calculateSums(data: List[Int]): Set[Int] = {

    @tailrec
    def calculate(head: Int, numbersToUse: List[Int], calculated: Set[Int]): Set[Int] = {
      val recalculatedValues = calculated ++ calculated.map(_ + head) + head
      numbersToUse match {
        case Nil => recalculatedValues
        case head :: tail => calculate(head, tail, recalculatedValues)
      }
    }

    calculate(data.head, data.tail, Set(0))
  }
}
