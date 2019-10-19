package exercises

import scala.io.Source
import scala.util.Using

object BlackWhiteGrid extends App {

  //  val n = 4;
  //  val matrix: Array[Array[Char]] = Array(
  //    Array('w', 'w', 'w', 'w'),
  //    Array('b', 'w', 'w', 'b'),
  //    Array('b', 'w', 'w', 'w'),
  //    Array('w', 'b', 'b', 'b'))

  val matrix: Array[Array[Char]] = Using(Source.fromFile("data/grid-2000x2000.txt"))(
    bufferedSource => bufferedSource.getLines.toArray.map(_.toCharArray)
  ).get
  val n = matrix.length

  val st = System.currentTimeMillis()
  var countMatrix = Array.fill(n, n)(0)

  for (row <- matrix.indices) {
    for (col <- row until matrix(row).length) {
      countMatrix(row)(col) = calcSquaresOfCell(row, col)
      countMatrix(col)(row) = calcSquaresOfCell(col, row)
    }
  }
  private val sum: Int = countMatrix.map(_.sum).sum
  val en = System.currentTimeMillis()
  println(s"Calculated $sum squares in: ${en - st} ms")

  def calcSquaresOfCell(row: Int, col: Int): Int =
    if (isWhite(row, col)) calculateBasedOnPrevious(row, col) else 0

  def isWhite(row: Int, col: Int): Boolean = matrix(row)(col) == 'W'

  def calculateBasedOnPrevious(row: Int, col: Int): Int = {
    val smallestPreviousValue = (row, col) match {
      case (_, 0) => 0
      case (0, _) => 0
      case (r, c) => (countMatrix(r - 1)(c - 1) min countMatrix(r)(c - 1) min countMatrix(r - 1)(c))
    }
    smallestPreviousValue + 1
  }

}
