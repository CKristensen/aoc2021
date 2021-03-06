ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "me.cassayre.florian"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AdventOfCode-2019",
    description := "Advent of Code 2019",
    version := "0.1.0",
 )


commands += Command("day") { _ =>
  import complete.DefaultParsers._
  (' ' ~ charClass(_.isDigit, "digit").+.map(_.mkString.toInt)).map(_._2)
} { case (previousState, i: Int) =>
  val formatted = "%02d".format(i)
  Command.process(s"runMain adventofcode.solutions.Day$formatted", previousState)
}

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
