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

  private val rotatedMatrix: Array[Array[Char]] = Array.ofDim(matrix.length, matrix(0).length)
  val start = System.currentTimeMillis()

  for (i <- matrix.indices) {
    for (j <- i until matrix(i).length) {
      rotatedMatrix(j)(i) = matrix(i)(j);
      rotatedMatrix(i)(j) = matrix(j)(i);
    }
  }

  val result = matrixContainsStringInRow(matrix) || matrixContainsStringInRow(rotatedMatrix)

  val end = System.currentTimeMillis()
  println(s"Took ${end - start} ms with result: ${result}")

  def matrixContainsStringInRow(mx: Array[Array[Char]]): Boolean = {
    for (row <- mx) {
      if (containsString(row)) {
        return true
      }
    }
    false
  }

  def containsString(array: Array[Char]): Boolean = {
    array.mkString.indexOfSlice(expectedWord) != -1
  }
}
