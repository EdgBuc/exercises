package exercises

import scala.annotation.tailrec
import scala.math._

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
