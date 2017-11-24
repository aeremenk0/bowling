package ru.eremenko.bowling


sealed trait Frame
case class Regular(first: Int, second: Int) extends Frame
case class Spare(first: Int, second: Int) extends Frame
case class Strike(first: Int = Bowling.PinNumber) extends Frame
case class LastFrame(first: Int, second: Int, third: Option[Int]) extends Frame

case class Accumulator(score: Int, last: Option[Int], secondLast: Option[Int])

object Bowling {
  val PinNumber: Int = 10
  val FramesNumber: Int = 10

  def calc(frame: Frame, acc: Accumulator): Either[String, Accumulator] =
    frame match {
      case Strike(f) =>
        acc match {
          case Accumulator(score, Some(last), Some(secondLast)) =>
            Right(Accumulator(score + f + last + secondLast, Some(f), Some(last)))
          case Accumulator(score, sl, None) =>
            Right(Accumulator(score, Some(f), sl))
          case _ => Left("Accumulator invariant violation")
        }
      case Spare(f, s) =>
        acc match {
          case Accumulator(score, Some(last), _) =>
            Right(Accumulator(score + f + s + last, Some(f), Some(last)))
          case Accumulator(score, None, None) =>
            Right(Accumulator(score, Some(f+s), None))
          case _ => Left("Accumulator invariant violation")
        }
      case Regular(f, s) => Right(Accumulator(acc.score + f + s, Some(f), Some(s)))
      case LastFrame(f, s, t) => Right(Accumulator(acc.score + f + s + t.getOrElse(0), Some(f), Some(s)))
    }

  def isLastFrame(fr: Frame): Boolean = {
    fr match {
      case LastFrame(_, _, _) => true
      case _ => false
    }
  }

  def isValidFrame(fr: Frame): Boolean = {
    fr match {
      case Strike(f) => f == PinNumber
      case Spare(f, s) => f + s == PinNumber && s > 0 & f > 0
      case Regular(f, s) => f + s < PinNumber && f >= 0 && s >= 0
      case LastFrame(f, s, t) =>
        if(f + s < PinNumber) t.isEmpty
        else if (f + s > PinNumber * 2) false
        else t.isDefined
    }
  }

  def isValidGame(g: List[Frame]) : Boolean = {
    val r = if(g.length == FramesNumber) isLastFrame(g.last) else true

    isValidPartialGame(g) && r
  }

  def isValidPartialGame(game: List[Frame]) : Boolean = {
    game.length <= FramesNumber && game.forall(isValidFrame)
  }

  def score(game: List[Frame]): Either[String, Int] = {
    val acc = game.foldRight(Right(Accumulator(0, None, None)): Either[String, Accumulator]){
      (fr, acc) =>  acc.flatMap(a => calc(fr, a))
    }

    acc.map(_.score)
  }

  def prepareString(s: String) : List[Char] = {
    s.to[List].filterNot(_ == ' ').map( c => if(c == '-') '0' else c)
  }

  def charToInt(c: Char) : Either[Error, Int] = {
    if(c == 'X') Right(PinNumber)
    else if(c == '-') Right(0)
    else if(c.isDigit) Right(c.asDigit)
    else Left(new Error("$c can not to be parsed to digit"))
  }

  def parse(s: String) : Either[Error, List[Frame]] = {
    def go(ls: List[Char], acc: List[Frame], nodes: Int = 0): Either[Error, List[Frame]] = {
      ls match {
        case Nil => Right(acc)
        case 'X' :: 'X' :: n :: Nil if nodes == FramesNumber - 1 =>
          charToInt(n).map(d => LastFrame(PinNumber, PinNumber, Option(d)) :: acc)

        case s :: '/' :: n :: Nil if nodes == FramesNumber - 1 =>
          for{
            d1 <- charToInt(s)
            d2 <- charToInt(n)
          } yield (LastFrame(d1, PinNumber - d1, Option(d2)) :: acc)

        case s :: n :: Nil if nodes == FramesNumber - 1 =>
          for{
            d1 <- charToInt(s)
            d2 <- charToInt(n)
          } yield (LastFrame(d1, d2, None) :: acc)

        case 'X' :: tail => go(tail, Strike() :: acc, nodes + 1)
        case n :: '/' :: tail =>
          charToInt(n).flatMap(d => go(tail, Spare(d, PinNumber - d) :: acc, nodes + 1))

        case s :: n :: tail =>
          val r = for {
            d1 <- charToInt(s)
            d2 <- charToInt(n)
          } yield (d1, d2)

          r.flatMap(d => go(tail, Regular.tupled(d) :: acc, nodes + 1))

        case _ =>
          Left(new Error("Invalid input"))
      }
    }
    go(prepareString(s), Nil).map(_.reverse)
  }

  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")

    val r = Bowling.parse(input)

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res match {
      case Right(s) => println(s)
      case Left(e) => println(e)
    }
  }
}
