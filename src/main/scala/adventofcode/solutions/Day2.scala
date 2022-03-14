package adventofcode.solutions

import adventofcode.Day

object Day02 extends Day(2) {

  val data: IndexedSeq[String] = lines

  val calcA = lines
    .map(_.split(' '))
    .map { a: Array[String] =>
      a.head match {
        case "forward" => (a.tail.head.toInt, 0)
        case "up"      => (0, -(a.tail.head.toInt))
        case "down"    => (0, a.tail.head.toInt)
        case _         => (0, 0)
      }
    }
    .reduce((b1, b2) => (b1._1 + b2._1, b1._2 + b2._2))

  override def solutionA = calcA._1 * calcA._2

  def auxf(op: String, x: Int, aim: Int, depth: Int, horizontal: Int) =
    op match {
      case "forward" => (aim, depth + (aim * x), horizontal + x)
      case "up"      => (aim - x, depth, horizontal)
      case "down"    => (aim + x, depth, horizontal)
    }

  val calcB = lines
    .map(_.split(' '))
    .map(a => (a.head, a.tail.head.toInt))
    .foldLeft((0, 0, 0))((b, a) => auxf(a._1, a._2, b._1, b._2, b._3))

  override def solutionB = calcB._3 * calcB._2
}
