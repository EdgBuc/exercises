package exercises

import scala.annotation.tailrec

object SubsetsSums extends App {

  def calculateSums(data: List[Int]): Set[Int] = {

    @tailrec
    def calculate(head: Int, tail: List[Int], calculated: Set[Int]): Set[Int] = {
      val newValues = calculated.map(_ + head)
      val values = calculated ++ newValues + head

      if (tail.isEmpty) {
        values
      } else {
        calculate(tail.head, tail.tail, values)
      }
    }

    calculate(data.head, data.tail, Set(0))
  }
}
