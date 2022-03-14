package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  val data: IndexedSeq[Int] = lines.map(_.toInt)
  val window3 =
    data.zip(data.tail).zip(data.tail.tail).map(a => a._1._1 + a._1._2 + a._2)

  override def solutionA = data.zip(data.tail).filter(a => a._1 < a._2).length

  override def solutionB =
    window3.zip(window3.tail).filter(a => a._1 < a._2).length

}
