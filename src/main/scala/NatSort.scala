package main.scala

object NatSort {
  def sort(input: Traversable[String]): Traversable[String] = {
    val mapped = for {
      current <- input
    } yield (current, current.toDouble)

    val sorted = mapped.toList.sortBy(x => x._2)

    val reduced = for {
      current <- sorted
    } yield current._1

    reduced
  }
}
