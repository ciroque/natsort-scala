package main.scala

import org.specs2.mutable._
import scala.util.Random

class NatSortSpec extends Specification {

  "NatSort " should {
    "- Return correct sorting given a simple list of numbers " in {

      val input = List("1000", "0200", "1", "1000000", "007")
      val expected = List("1", "007", "0200", "1000", "1000000")

      val output = NatSort.sort(input.toTraversable)

      assertSorted(expected, output.toIterable)
    }

    "- Blow up when an element is not Int parseable" in {
      val input = List("1000", "0200", "1", "b1000000", "007")

      val result = try {
        NatSort.sort(input.toTraversable)
      } catch {
        case t: Throwable =>
          t.getMessage
      }

      result.toString must contain("b1000000")
    }

    "- Correct the order of a randomized list" in {
      val expected = List("2", "44", "060", "00100", "157", "001230", "5000", "6513", "10000", "123456789", "987654321")
      val randomized = Random.shuffle(expected)
      val sorted = NatSort.sort(randomized)

      assertSorted(expected, sorted.toIterable)
    }

    "- Handle floating point numbers" in {
      val expected = List("2.02", "44", "060", "00100.25", "157.99", "001230", "5000", "6513.71", "6513.89", "10000", "123456789", "987654321")
      val randomized = Random.shuffle(expected)
      val sorted = NatSort.sort(randomized)

      sorted should be(expected)
//      assertSorted(expected, sorted.toIterable)
    }

    "- Handle filenames" in {
      val expected = List("ver-1.2.1", "ver-1.2.3", "ver-1.2.5", "ver-1.2.15", "ver-1.3.3", "ver-1.3.12")
      val randomize = Random.shuffle(expected)
      val sorted = NatSort.sort(randomize)

      assertSorted(expected, sorted.toIterable)
    }

    "- Handle chemical elements" in {
      val expected = List("C1H2", "C1H4", "C2H2", "C2H6", "C2N", "C3H6")
      val randomize = Random.shuffle(expected)
      val sorted = NatSort.sort(randomize)

      assertSorted(expected, sorted.toIterable)
    }

    "- Handle team names" in {
      val expected = List("Team 1", "Team 30", "Team 58", "Team 101")
      val randomize = Random.shuffle(expected)
      val sorted = NatSort.sort(randomize)

      assertSorted(expected, sorted.toIterable)
    }
  }

  private def assertSorted(expected: Iterable[String], actual: Iterable[String]) = {
    val zipped = expected.zip(actual)

    zipped.map(zip => println(s"> ${zip._1} --> ${zip._2} <"))

    val filtered = zipped.filter(strs => strs._1 != strs._2)
    val differences = filtered.count(str => true)

    differences should beEqualTo(0)
  }
}
