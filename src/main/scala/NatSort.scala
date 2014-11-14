package main.scala

import scala.util.matching.Regex

object ListOrdering extends Ordering[List[Long]] {
  implicit override def compare(x: List[Long], y: List[Long]): Int = {
    val left = x.reverse
    val right = y.reverse

    left.zip(right).foldLeft(0) { case (acc, lr) => acc + lr._1.compareTo(lr._2) }
  }
}

object NatSort {
  private val numberExtraction = new Regex("\\d+")

  def sort(input: Traversable[String]): Traversable[String] = {

    object WordNumber
    case class WordNumber(word: String, number: List[Long])

    def extractNumeric(word: String): WordNumber = {
      val matches = numberExtraction.findAllMatchIn(word)
      val matchList = matches.toList

      val theseNumbers = matchList.map(ma => ma.group(0).toLong)

      WordNumber(word, theseNumbers)
    }

    val mapped = for {
      current <- input
    } yield extractNumeric(current)

    val sorted = mapped.toList.sortBy(wn => wn.number)(ListOrdering)

    val reduced = for {
      current <- sorted
    } yield current.word

    reduced
  }
}
