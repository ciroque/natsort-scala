package main.scala

import scala.util.matching.Regex

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

    val sorted = mapped.toList.sortBy { wn => wn.number.sum }

    val reduced = for {
      current <- sorted
    } yield current.word

    reduced
  }
}
