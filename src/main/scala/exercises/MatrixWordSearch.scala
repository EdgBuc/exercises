package exercises

import scala.io.Source
import scala.util.Using

object MatrixWordSearch extends App {

  //  val matrix: Array[Array[Char]] = Array(
  //    Array('F', 'O', 'A', 'M'),
  //    Array('O', 'B', 'Q', 'P'),
  //    Array('F', 'F', 'T', 'B'),
  //    Array('M', 'A', 'S', 'S'))
  //  val expectedWord = "FOAM"

  val expectedWord: String = Using(Source.fromFile("data/search-string.txt"))(
    bufferedSource => bufferedSource.getLines.mkString
  ).get

  val matrix: Array[Array[Char]] = Using(Source.fromFile("data/matrix2000x2000.txt"))(
    bufferedSource => bufferedSource.getLines.toArray.map(_.toCharArray)
  ).get

  val start = System.currentTimeMillis()

  val expectedWithIndex: Array[(Char, Int)] = expectedWord.toCharArray.zipWithIndex
  val indexedMatrix1D = matrix.flatten.zipWithIndex

  val coordinatesOfMaybeWordStart = indexedMatrix1D.filter {
    case (letter, idx) =>
      val (row, col) = coordinatesInMatrix(idx)
      (willFitHorizontally(row, col) || willFitVertically(row, col)) && letter == expectedWord.head
  }.map {
    case (_, idx) => coordinatesInMatrix(idx)
  }

  val result: Boolean = coordinatesOfMaybeWordStart.exists { case (row, col) => isWordHere(row, col) }

  val end = System.currentTimeMillis()
  println(s"Took ${end - start} ms with result: ${result}")

  def willFitHorizontally(row: Int, col: Int) = col + expectedWord.length <= matrix(row).length

  def willFitVertically(row: Int, col: Int) = row + expectedWord.length <= matrix.length

  def isWordHere(row: Int, col: Int): Boolean = {
    isWordVertical(row, col) || isWordHorizontal(row, col)
  }

  def isWordVertical(row: Int, col: Int): Boolean = {
    if (row + expectedWord.length <= matrix.length) {
      wordMatch(matrix(_)(col))
    } else {
      false
    }
  }

  def isWordHorizontal(row: Int, col: Int): Boolean = {
    if (col + expectedWord.length <= matrix(row).length) {
      wordMatch(matrix(row)(_))
    } else {
      false
    }
  }

  def coordinatesInMatrix(idx: Int): (Int, Int) = {
    val row = idx / matrix(0).length
    val col = idx - row * matrix(0).length
    (row, col)
  }

  def wordMatch(actualChar: Int => Char): Boolean = {
    for ((letter, i) <- expectedWithIndex) {
      if (actualChar(i) != letter) {
        return false
      }
    }
    true
  }
}
