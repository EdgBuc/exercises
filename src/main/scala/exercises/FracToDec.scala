package exercises

import scala.collection.mutable

object FracToDec extends App {
  println(calc(1, 10000025))

  def calc(sNumerator: Long, sDenominator: Long): String = {
    val result = new StringBuilder()
    if ((sNumerator < 0 && sDenominator > 0) || (sNumerator > 0 && sDenominator < 0)) {
      result.append("-")
    }
    val numerator = Math.abs(sNumerator)
    val denominator = Math.abs(sDenominator)

    result.append(numerator / denominator)
    var rest = numerator % denominator;
    if (rest != 0) {
      result.append(",")
      val calculated = mutable.Map[(Long, Long), Int]()
      while (rest != 0) {
        rest *= 10;
        val n = rest / denominator
        if (calculated.contains((rest, n))) {
          result.insert(calculated((rest, n)), "(")
          result.append(")")
          return result.toString()
        } else {
          calculated.put((rest, n), result.length())
        }
        result.append(n)
        rest = rest % denominator
      }
    }
    result.toString()
  }
}
