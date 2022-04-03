package adventofcode.solutions

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object Day4Spec extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("group exists") = forAll { (a: Int) =>
    val line1: Line = Line(List(a), List())
    val line2: Line = Line(List(), List())
    Group(line1 :: line2 :: Nil).exists(a)
  }
  property("group bingo") = forAll { (a: Int, b: Int, c: Int) =>
    val line1: Line = Line(List(a, c), List())
    val line2: Line = Line(List(b, a, c), List())
    Group(line1 :: line2 :: Nil).draw(c).draw(a).bingo()
  }
  property("list group bingo") = forAll { (a: Int, b: Int, c: Int) =>
    val line1: Line = Line(List(a, c), List())
    val line2: Line = Line(List(b, a, c), List())
    List(Group(line1 :: line2 :: Nil).draw(c).draw(a)).exists(_.bingo())
  }
  property("group unmarked sum") = forAll { (a: Int, b: Int, c: Int) =>
    val line1: Line = Line(List(c), List())
    val line2: Line = Line(List(b, a), List())
    Group(line1 :: line2 :: Nil).unmarked_sum() == a + b + c
  }
}
