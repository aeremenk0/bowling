package ru.eremenko.bowling

sealed trait Frame
case class Regular(first: Int, second: Int) extends Frame
case class Spare(first: Int, second: Int) extends Frame
case class Strike(first: Int = 10) extends Frame
case class LastFrame(first: Int, second: Int, third: Option[Int]) extends Frame

case class Accumulator(score: Int, last: Option[Int], secondLast: Option[Int])

object Bowling {
  def calc(frame: Frame, acc: Accumulator): Accumulator =
    frame match {
      case Strike(f) =>
        acc match {
          case Accumulator(score, Some(last), Some(secondLast)) =>
            Accumulator(score + f + last + secondLast, Some(f), Some(last))
          case Accumulator(score, sl, None) =>
            Accumulator(score, Some(f), sl)
        }
      case Spare(f, s) =>
        acc match {
          case Accumulator(score, Some(last), _) =>
            Accumulator(score + f + s + last, Some(f), Some(last))
          case Accumulator(score, None, None) =>
            Accumulator(score, Some(f+s), None)
        }
      case Regular(f, s) => Accumulator(acc.score + f + s, Some(f), Some(s))
      case LastFrame(f, s, t) => Accumulator(acc.score + f + s + t.getOrElse(0), Some(f), Some(s))
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
      case Spare(f, s) => f + s == 10 && s > 0 & f > 0
      case Regular(f, s) => f + s < 10 && f > 0 && s > 0
      case LastFrame(f, s, t) =>
        if(f + s < 10) t.isEmpty
        else if (f + s > 20) false
        else t.isDefined
    }
  }

  def validateGame(game: List[Frame]) : Option[List[Frame]] = {
    validatePartialGame(game).flatMap{ g =>
      if (g.length == 10 && isLastFrame(g.last)) Option(g)
      else None
    }
  }

  def validatePartialGame(game: List[Frame]) : Option[List[Frame]] = {
    if (game.length <= 10 && game.forall(checkFrame)) Option(game)
    else None
  }

  def score(game: List[Frame]): Int = {
    val acc = game.foldRight(Accumulator(0, None, None))(calc(_,_))

    acc.score
  }
}
