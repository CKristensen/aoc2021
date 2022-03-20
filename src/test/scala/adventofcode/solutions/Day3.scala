package adventofcode.solutions

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object StringSpecification extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  val bit             = Gen.choose(0, 1)
  def listBit(n: Int) = Gen.listOfN(n, bit)

  property("basic common") = forAll(listBit(1000)) { n =>
    Day03.least_common(n) != Day03.most_common(n)
  }

  def homeMadeBitConverter(b: Tuple2[Int, Int]): Int =
    b._1 * scala.math.pow(2, b._2).toInt

  property("bitToInt") = forAll(listBit(12)) { n: List[Int] =>
    val day3 = Day03.bitStringToInt(n.map(_.toInt)).reduce(_ + _).toInt
    val manual = n.reverse
      .map(_.toInt)
      .zipWithIndex
      .map(homeMadeBitConverter(_))
      .reduce(_ + _)

    day3 == manual
  }
}
