package exercises

import scala.collection.immutable.Queue
import scala.util.Either

case class Diff(add: String, remove: String)

object DynamicProgramming {
  def apply(source: String, target: String): String = {
    (source, target) match {
      case (s, t) if (s.isEmpty && t.isEmpty) => "";
      case (s, t) => makeDiffString(compare(s.split("\n").toList, t.split("\n").toList))
    }
  }

  def compare(source: List[String], target: List[String]): Queue[Either[String, Diff]] = {
    (source, target) match {
      case (Nil, Nil) => Queue.empty
      case (Nil, head :: tail) => Left(transformToAddLine(head)) +: compare(source, tail)
      case (head :: tail, Nil) => Left(transformToDropLine(head)) +: compare(tail, target)
      case (sourceHead :: sourceTail, targetHead :: targetTail) =>
        if (sourceHead == targetHead)
          Left(transformToEqlLine(sourceHead)) +: compare(sourceTail, targetTail)
        else
          Right(Diff(transformToAddLine(targetHead), transformToDropLine(sourceHead))) +: compare(sourceTail, targetTail)
    }
  }

  def normalizeOutput(comparison: Queue[Either[String, Diff]]): Queue[Either[String, Diff]] = {
    comparison.headOption match {
      case None => Queue.empty
      case l@Some(Left(_)) => l.get +: normalizeOutput(comparison.drop(1))
      case a@Some(Right(Diff(add, remove))) =>
        val normalized = normalizeOutput(comparison.drop(1))
        normalized.headOption match {
          case Some(Right(Diff(nAdd, nRemove))) =>
            Right(Diff(s"$add\n$nAdd", s"$remove\n$nRemove")) +: normalized.drop(1)
          case _ => a.get +: normalized
        }
    }
  }

  def makeDiffString(comparison: Queue[Either[String, Diff]]): String = {
    val value = normalizeOutput(comparison)
    value.map {
      case Left(str) => str
      case Right(Diff(add, remove)) => s"$remove\n$add"
    }.mkString("\n") + "\n "
  }

  def transformToAddLine(string: String): String = s"+$string"

  def transformToDropLine(string: String): String = s"-$string"

  def transformToEqlLine(string: String): String = s" $string"
}
