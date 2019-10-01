package exercises

import java.util.Comparator
import com.google.common.collect.TreeMultiset
import org.apache.commons.lang3.math.NumberUtils
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.math._
import scala.util.Using
import scala.io.Source


object Hotelier extends App {

  def generate(input: String, numOfRooms: Int = 10): Long = {
    val inputArray = input.toCharArray
    val tempResult = pow(10, numOfRooms).toLong

    var result = tempResult
    for (event <- inputArray) {
      result = resolveEvent(result, event, numOfRooms)
    }
    result % tempResult
  }

  private def resolveEvent(result: Long, event: Char, numOfRooms: Int): Long =
    event match {
      case 'L' => applyEvent(result, numOfRooms - 1, _ - 1)
      case 'R' => applyEvent(result, 0, _ + 1)
      case d if Character.isDigit(d) => leaveEvent(result, d.asDigit, numOfRooms)
    }

  @tailrec
  private def applyEvent(result: Long, position: Int, next: Int => Int): Long = {
    val positionNumber = pow(10, position).toLong
    ((result / positionNumber) % 10) match {
      case 0 => result + positionNumber
      case _ => applyEvent(result, next(position), next)
    }
  }

  private def leaveEvent(result: Long, num: Int, numOfRooms: Int): Long =
    result - pow(10, numOfRooms - 1 - num).toLong
}


object SuperMegaHotelier extends App {

  val totalRooms = 1000000
  var inputArray: Array[String] = Array.empty

  Using(Source.fromFile("data/hotelier-large.txt"))(bufferedSource =>
    inputArray = bufferedSource.getLines.toArray
  )

  val ascComp: Comparator[Int] = (o1, o2) => o1 - o2
  private val positions = TreeMultiset.create(ascComp)
  private val list: List[Int] = (0 until totalRooms).toList
  positions.addAll(list.asJava)

  private val st: Long = System.currentTimeMillis()

  for (event <- inputArray) {
    event match {
      case "L" => positions.pollFirstEntry()
      case "R" => positions.pollLastEntry()
      case d if NumberUtils.isParsable(d) => positions.add(d.toInt)
    }
  }

  private val result: Array[Int] = Array.fill(totalRooms)(1)
  positions.forEach(p => {
    result(p) = 0
  })

  private val en: Long = System.currentTimeMillis()
  println("Time taken: ${en - st}")
  println(result.mkString(""))
}
