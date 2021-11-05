package exercises

import scala.annotation.tailrec


object SquaringSortedArray {

  /**
   * Given a sorted array, create a new array containing squares of all the numbers of the input array in the sorted order.
   *
   * Solution should have O(N) time complexity
   *
   * @param input sorted array
   * @return sorted squares of input array
   */
  def apply(input: Array[Int]): Array[Int] = {

    @tailrec
    def calculateSortedSquares(left: Int, right: Int, collection: List[Int] = Nil): List[Int] = {
      if(left > right){
        collection
      }else if (sqr(input(left))>sqr(input(right))){
        calculateSortedSquares(left +1, right, sqr(input(left)) +:collection )
      }else {
        calculateSortedSquares(left, right-1, sqr(input(right)) +:collection )
      }
    }

    calculateSortedSquares(0, input.length-1).toArray
  }

  private def sqr(x: Int) = x * x

}
