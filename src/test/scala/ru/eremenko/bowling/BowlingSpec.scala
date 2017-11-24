package ru.eremenko.bowling

import org.scalatest.{FlatSpec, Matchers}


class BowlingSpec extends FlatSpec with Matchers {
  "Bowling.score" should "return score 0 for an empty input" in {
    val r = Bowling.score(List.empty)
    r shouldBe Right(0)
  }

  "Bowling.score" should "return score 30 for LastFrame of strikes" in {
    val r = Bowling.score(List(LastFrame(10,10,Option(10))))
    r shouldBe Right(30)
  }

  "Bowling.score" should "return score 9 for Regular frame" in {
    val r = Bowling.score(List(LastFrame(4,5,None)))
    r shouldBe Right(9)
  }

  "Bowling.score" should "return score 19 for Regular + Spare sequence" in {
    val r = Bowling.score(List(Spare(5,5), Regular(2,5)))
    r shouldBe Right(12 + 7)
  }

  "Bowling.score" should "return score 21 for Regular + Strike sequence" in {
    val r = Bowling.score(List(Strike(), Regular(2,5)))
    r shouldBe Right(10 + 2 + 5 + 7)
  }

  "Bowling.score" should "return score 300 for all Strikes sequence" in {
    val game = ( LastFrame(10, 10, Option(10)) :: List.fill(9)(Strike())).reverse

    val r = Bowling.score(game)
    r shouldBe Right(300)
  }

  "Bowling.score" should "return score 90 for all 10xRegular(4,5) sequence" in {
    val game = ( LastFrame(4, 5, None) :: List.fill(9)(Regular(4,5))).reverse

    val r = Bowling.score(game)
    r shouldBe Right(90)
  }

  "Bowling.score" should "return score 150 for all Spare(5,5) sequence" in {
    val game = ( LastFrame(5, 5, Option(5)) :: List.fill(9)(Spare(5,5))).reverse

    val r = Bowling.score(game)
    r shouldBe Right(150)
  }

  "Bowling.isLastFrame" should "return true for LastFrame" in {
    Bowling.isLastFrame(LastFrame(1,1,None)) shouldBe true
  }

  "Bowling.isLastFrame" should "return false for any other Frame" in {
    Bowling.isLastFrame(Strike()) shouldBe false
    Bowling.isLastFrame(Spare(1,9)) shouldBe false
    Bowling.isLastFrame(Regular(1,2)) shouldBe false
  }

  "Bowling.checkFrame" should "return true if the frame is correct" in {
    Bowling.isValidFrame(Strike()) shouldBe true
    Bowling.isValidFrame(Spare(5,5)) shouldBe true
    Bowling.isValidFrame(Regular(3,5)) shouldBe true
    Bowling.isValidFrame(LastFrame(3,3, None)) shouldBe true
    Bowling.isValidFrame(LastFrame(5,5, Option(5))) shouldBe true
    Bowling.isValidFrame(LastFrame(10,10, Option(10))) shouldBe true
  }

  "Bowling.checkFrame" should "return false if the frame is not correct" in {
    Bowling.isValidFrame(Strike(1)) shouldBe false
    Bowling.isValidFrame(Spare(1,5)) shouldBe false

    Bowling.isValidFrame(Regular(10,5)) shouldBe false
    Bowling.isValidFrame(Regular(-1,5)) shouldBe false
    Bowling.isValidFrame(Regular(-1, -5)) shouldBe false
    Bowling.isValidFrame(Regular(1, -5)) shouldBe false

    Bowling.isValidFrame(LastFrame(3,3, Option(1))) shouldBe false
    Bowling.isValidFrame(LastFrame(11, 12, Option(1))) shouldBe false

    Bowling.isValidFrame(LastFrame(5,5, None)) shouldBe false
    Bowling.isValidFrame(LastFrame(10,10, None)) shouldBe false
  }

  "Bowling.isValidGame" should "return true if partial game is valid" in {
    Bowling.isValidGame(List(Strike())) shouldBe true

    val game = List.fill(10)(Spare(5,5))
    Bowling.isValidGame(game) shouldBe false

    val g2 = ( LastFrame(5, 5, Option(5)) :: List.fill(10)(Spare(5,5))).reverse
    Bowling.isValidGame(g2) shouldBe false

    val g3 = ( LastFrame(5, 5, Option(5)) :: Strike(1) :: List.fill(8)(Spare(5,5))).reverse
    Bowling.isValidGame(g3) shouldBe false
  }

  "Bowling.isValidGame" should "return Some[List[Frame]] if the game is valid" in {
    val g = ( LastFrame(5, 5, Option(5)) :: List.fill(9)(Spare(5,5))).reverse
    Bowling.isValidGame(g) shouldBe true
  }

  "Bowling.score" should "return 125 for partial game of Spares" in {
    val g = (List.fill(9)(Spare(5,5))).reverse
    Bowling.score(g) shouldBe Right(125)
  }

  "Bowling.score" should "return 270 for partial game of Strikes" in {
    val g = (List.fill(9)(Strike())).reverse
    Bowling.score(g) shouldBe Right(210)
  }

  "Bowling.score" should "return 10 for partial game of Regular" in {
    val g = List(Regular(1,2), Regular(3, 4))
    Bowling.score(g) shouldBe Right(10)
  }

  "Bowling.score" should "return 10 for partial game of Regular + Strike" in {
    val g = List(Regular(1,2), Regular(3, 4), Strike())
    Bowling.score(g) shouldBe Right(10)
  }

  "Bowling.score" should "return 30 for partial game of Regular + Strike + Regular" in {
    val g = List(Regular(1,2), Regular(3, 4), Strike(), Regular(2, 3))
    Bowling.score(g) shouldBe Right(30)
  }

  "Bowling.score" should "return 10 for partial game of Regular + Spare" in {
    val g = List(Regular(1,2), Regular(3, 4), Spare(5, 5))
    Bowling.score(g) shouldBe Right(10)
  }

  "Bowling.score" should "return 14 for partial game of Regular + Spare + Regular" in {
    val g = List(Regular(1,2), Regular(3, 4), Spare(5, 5), Regular(2,3))
    Bowling.score(g) shouldBe Right(27)
  }

  "Bowling.prepareString" should "return List[Char] with removed spaces" in {
    val r = Bowling.prepareString("X X X X X X X X X X X X")
    r.size shouldBe 12
    r.forall(_ == 'X')
  }

  "Bowling.prepareString" should "return List[Char] with removed spaces and - converted into 0" in {
    val r = Bowling.prepareString("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-")
    r.size shouldBe 20
  }

  "Bowling.parse" should "return Right[Nil] on empty input" in {
    Bowling.parse("") shouldBe Right(Nil)
  }

  "Bowling.parse" should "return Right[List[Char]] on valid input" in {
    val r = Bowling.parse("X X X X X X X X X X X X")

    r shouldBe 'right
  }

  "Bowling.score" should "return Right(300) for all Strikes" in {
    val r = Bowling.parse("X X X X X X X X X X X X")

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe Right(300)
  }

  "Bowling.parse" should "return Right[List[Char]] on valid input 2" in {
    val r = Bowling.parse("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-")

    r shouldBe 'right
  }

  "Bowling.score" should "return Right(90) for all 9-" in {
    val r = Bowling.parse("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-")

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe Right(90)
  }

  "Bowling.parse" should "return Try[List[Char]] on valid input 3" in {
    val r = Bowling.parse("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5")

    r shouldBe 'right
  }

  "Bowling.score" should "return Right(125) for all 5/" in {
    val r = Bowling.parse("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5")

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe Right(150)
  }

  "Bowling.score" should "return Right(15) for all 5/" in {
    val r = Bowling.parse("5/ 5/")

    val res = r.flatMap{g =>
      if(Bowling.isValidPartialGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe Right(20)
  }

  "Bowling.score" should "return Right(0) for empty input" in {
    val r = Bowling.parse("")

    val res = r.flatMap{g =>
      if(Bowling.isValidPartialGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe Right(0)
  }

  "Bowling.score" should "return Left for invalid input" in {
    val r = Bowling.parse("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 535")

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe 'left
  }

  "Bowling.score" should "return Left for invalid input ..." in {
    val r = Bowling.parse("5// 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5// 53")

    val res = r.flatMap{g =>
      if(Bowling.isValidGame(g))
        Bowling.score(g)
      else
        Left(new Error("Game is not valid."))
    }

    res shouldBe 'left
  }
}
