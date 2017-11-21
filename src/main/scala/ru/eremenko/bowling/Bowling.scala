package ru.eremenko.bowling

sealed trait Frame
case class Regular(first: Int, second: Int) extends Frame
case class Spare(first: Int, second: Int) extends Frame
case class Strike(first: Int = 10) extends Frame
case class LastFrame(first: Int, second: Int, third: Option[Int]) extends Frame

case class Accumulator(score: Int, last: Int, secondLast: Int)

object Bowling {
  def calc(frame: Frame, acc: Accumulator): Accumulator =
    frame match {
      case Strike(f) => Accumulator(acc.score + f + acc.last + acc.secondLast, f, acc.last)
      case Spare(f, s) => Accumulator(acc.score + f + s + acc.last, f, s)
      case Regular(f, s) => Accumulator(acc.score + f + s, f, s)
      case LastFrame(f, s, t) => Accumulator(acc.score + f + s + t.getOrElse(0), f, s)
    }

  def isLastFrame(fr: Frame): Boolean = {
    fr match {
      case LastFrame(_, _, _) => true
      case _ => false
    }
  }

  def checkFrame(fr: Frame): Boolean = {
    fr match {
      case Strike(f) => f == 10
      case Spare(f, s) => f + s == 10
      case Regular(f, s) => f + s < 10 && f > 0 && s > 0
      case LastFrame(f, s, t) =>
        if(f + s < 10) t.isEmpty
        else if (f + s > 20) false
        else t.isDefined
    }
  }

  def validate(game: List[Frame]) : Option[List[Frame]] = {
    if (game.length != 10) None
    else if(!isLastFrame(game.last)) None
    else if(!game.forall(checkFrame)) None


    Option(game)
  }

  def score(game: List[Frame]): Int = {
    val acc = game.foldRight(Accumulator(0, 0, 0))(calc(_,_))

    acc.score
  }
}
