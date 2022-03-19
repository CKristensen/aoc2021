package adventofcode.solutions

import adventofcode.Day

object Day03 extends Day(3) {

  val data: IndexedSeq[String] = lines

  def bitStringToInt(bit: List[Int]): List[BigInt] = {
    for ((i, b) <- bit.reverse.zipWithIndex)
      yield BigInt(i) * BigInt(scala.math.pow(2, b).toInt)
  }

  val bit_stream: IndexedSeq[List[Char]] =
    (for (i <- 0 to data.head.length - 1)
      yield (for (l <- data) yield l(i)).toList)

  def most_common(bits: List[Char]): Int = {
    var ones  = bits.filter(_ == '1').length
    var zeros = bits.filter(_ == '0').length
    if (ones < zeros) {
      0
    } else {
      1
    }
  }

  def least_common(bits: List[Char]): Int = {
    var ones  = bits.filter(_ == '1').length
    var zeros = bits.filter(_ == '0').length
    if (ones < zeros) {
      1
    } else {
      0
    }
  }

  val most_common_vals: List[Int] =
    (
      for (bit <- bit_stream)
        yield most_common(bit)
    ).toList

  val least_common_vals: List[Int] =
    (
      for (bit <- bit_stream)
        yield least_common(bit)
    ).toList

  //PART 1
  val gamma = bitStringToInt(
    most_common_vals
  ).reduce(_ + _)

  val epsilon = bitStringToInt(
    least_common_vals
  ).reduce(_ + _)

  override def solutionA = gamma * epsilon

  //PART 2
  def get_most_similar(
      d: List[List[Int]],
      sim: List[Int],
      i: Int
  ): List[Int] = {
    var filtered = d.filter(_(i) == sim(i)).toSet.toList
    if (filtered.length == 0) {
      d.head
    } else {
      get_most_similar(filtered, sim, i + 1)
    }
  }

  def stringToListInt(a: String): List[Int] = (
    for (b <- a) yield b.asDigit
  ).toList

  val oxygen_rating = bitStringToInt(
    get_most_similar(
      data.map(stringToListInt).toList,
      most_common_vals,
      0
    )
  ).reduce(_ + _)

  val co2_rating = bitStringToInt(
    get_most_similar(
      data.map(stringToListInt).toList,
      least_common_vals,
      0
    )
  ).reduce(_ + _)

  override def solutionB = oxygen_rating * co2_rating

}
