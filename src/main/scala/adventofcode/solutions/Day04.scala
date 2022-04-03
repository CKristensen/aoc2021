package adventofcode.solutions

import adventofcode.Day

case class Group(lines: List[Line]) {
  def exists(number: Int): Boolean = lines.exists(_.exists(number))
  def bingo(): Boolean             = lines.exists(_.bingo())
  def draw(i: Int): Group          = Group(lines.map(_.filter(i)))
  def unmarked_sum(): Int          = lines.map(_.unmarked_sum()).sum

}
case class Line(l: List[Int], drawn: List[Int]) {
  def filter(i: Int): Line = Line(
    l.filter(_ != i),
    if (l.exists(_ == i)) {
      i :: drawn
    } else drawn
  )
  def bingo(): Boolean        = l.length == 0
  def exists(i: Int): Boolean = l.exists(_ == i)
  def unmarked_sum(): Int     = l.sum
}

object Day04 extends Day(4) {

  val data: List[String]     = lines.toList
  val b_draws: List[Int]     = data.head.split(',').map(_.toInt).toList
  var allGroups: List[Group] = List()

  def get_first_bingo_score(draws: List[Int], groups: List[Group]): Int = {
    var aux_gp = groups
    var score  = 0
    for (d <- draws) {
      aux_gp = aux_gp.map(_.draw(d))
      if (aux_gp.exists(_.bingo())) {
        var bingoGroup = aux_gp.filter(_.bingo())(0)
        return bingoGroup.unmarked_sum() * d
      }
    }
    return score
  }

  def get_last_bingo_score(draws: List[Int], groups: List[Group]): Int = {
    var aux_gp = groups
    var score  = 0
    for (d <- draws) {
      aux_gp = aux_gp.map(_.draw(d))
      if (aux_gp.exists(_.bingo())) {
        var bingoGroup = aux_gp.filter(_.bingo()).head
        score = bingoGroup.unmarked_sum() * d
      }
      aux_gp = aux_gp.filter(!_.bingo())
    }
    return score
  }
  for (line <- data.tail) {
    if (line == "") {
      allGroups = Group(List()) :: allGroups
    } else {
      var incoming: Line = Line(
        line.split(' ').filter(_ != "").map(_.toInt).toList,
        List()
      )
      allGroups = Group(incoming :: allGroups.head.lines) :: allGroups.tail
    }
  }

  override def solutionA = get_first_bingo_score(b_draws, allGroups)
  override def solutionB = get_last_bingo_score(b_draws, allGroups)
}
