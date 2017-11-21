package ru.eremenko.bowling

import org.scalatest.{FlatSpec, Matchers}

class BowlingSpec extends FlatSpec with Matchers {
  "Bowling.score" should "return score 0 for an empty input" in {
    val r = Bowling.score(List.empty)
    r shouldBe 0
  }

  "Bowling.score" should "return score 30 for LastFrame of strikes" in {
    val r = Bowling.score(List(LastFrame(10,10,Option(10))))
    r shouldBe 30
  }

  "Bowling.score" should "return score 9 for Regular frame" in {
    val r = Bowling.score(List(LastFrame(4,5,None)))
    r shouldBe 9
  }

  "Bowling.score" should "return score 19 for Regular + Spare sequence" in {
    val r = Bowling.score(List(Spare(5,5), Regular(2,5)))
    r shouldBe (12 + 7)
  }

  "Bowling.score" should "return score 21 for Regular + Strike sequence" in {
    val r = Bowling.score(List(Strike(), Regular(2,5)))
    r shouldBe (10 + 2 + 5 + 7)
  }

  "Bowling.score" should "return score 300 for all Strikes sequence" in {
    val game = ( LastFrame(10, 10, Option(10)) :: List.fill(9)(Strike())).reverse

    val r = Bowling.score(game)
    r shouldBe 300
  }

  "Bowling.score" should "return score 90 for all 10xRegular(4,5) sequence" in {
    val game = ( LastFrame(4, 5, None) :: List.fill(9)(Regular(4,5))).reverse

    val r = Bowling.score(game)
    r shouldBe 90
  }

  "Bowling.score" should "return score 150 for all Spare(5,5) sequence" in {
    val game = ( LastFrame(5, 5, Option(5)) :: List.fill(9)(Spare(5,5))).reverse

    val r = Bowling.score(game)
    r shouldBe 150
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
    Bowling.checkFrame(Strike()) shouldBe true
    Bowling.checkFrame(Spare(5,5)) shouldBe true
    Bowling.checkFrame(Regular(3,5)) shouldBe true
    Bowling.checkFrame(LastFrame(3,3, None)) shouldBe true
    Bowling.checkFrame(LastFrame(5,5, Option(5))) shouldBe true
    Bowling.checkFrame(LastFrame(10,10, Option(10))) shouldBe true
  }

  "Bowling.checkFrame" should "return false if the frame is not correct" in {
    Bowling.checkFrame(Strike(1)) shouldBe false
    Bowling.checkFrame(Spare(1,5)) shouldBe false

    Bowling.checkFrame(Regular(10,5)) shouldBe false
    Bowling.checkFrame(Regular(-1,5)) shouldBe false
    Bowling.checkFrame(Regular(-1, -5)) shouldBe false
    Bowling.checkFrame(Regular(1, -5)) shouldBe false

    Bowling.checkFrame(LastFrame(3,3, Option(1))) shouldBe false
    Bowling.checkFrame(LastFrame(11, 12, Option(1))) shouldBe false

    Bowling.checkFrame(LastFrame(5,5, None)) shouldBe false
    Bowling.checkFrame(LastFrame(10,10, None)) shouldBe false
  }

  "Bowling.validateGame" should "return None if the game is not valid" in {
    Bowling.validateGame(List(Strike())) shouldBe None

    val game = List.fill(10)(Spare(5,5))
    Bowling.validateGame(game) shouldBe None

    val g2 = ( LastFrame(5, 5, Option(5)) :: List.fill(10)(Spare(5,5))).reverse
    Bowling.validateGame(g2) shouldBe None

    val g3 = ( LastFrame(5, 5, Option(5)) :: Strike(1) :: List.fill(8)(Spare(5,5))).reverse
    Bowling.validateGame(g3) shouldBe None
  }

  "Bowling.validateGame" should "return Some[List[Frame]] if the game is valid" in {
    val g = ( LastFrame(5, 5, Option(5)) :: List.fill(9)(Spare(5,5))).reverse
    Bowling.validateGame(g) shouldBe defined
  }

  "Bowling.score" should "return 125 for partial game of Spares" in {
    val g = (List.fill(9)(Spare(5,5))).reverse
    Bowling.score(g) shouldBe 125
  }

  "Bowling.score" should "return 270 for partial game of Strikes" in {
    val g = (List.fill(9)(Strike())).reverse
    Bowling.score(g) shouldBe 210
  }

  "Bowling.score" should "return 10 for partial game of Regular" in {
    val g = List(Regular(1,2), Regular(3, 4))
    Bowling.score(g) shouldBe 10
  }

  "Bowling.score" should "return 10 for partial game of Regular + Strike" in {
    val g = List(Regular(1,2), Regular(3, 4), Strike())
    Bowling.score(g) shouldBe 10
  }

  "Bowling.score" should "return 30 for partial game of Regular + Strike + Regular" in {
    val g = List(Regular(1,2), Regular(3, 4), Strike(), Regular(2, 3))
    Bowling.score(g) shouldBe 30
  }

  "Bowling.score" should "return 10 for partial game of Regular + Spare" in {
    val g = List(Regular(1,2), Regular(3, 4), Spare(5, 5))
    Bowling.score(g) shouldBe 10
  }

  "Bowling.score" should "return 14 for partial game of Regular + Spare + Regular" in {
    val g = List(Regular(1,2), Regular(3, 4), Spare(5, 5), Regular(2,3))
    Bowling.score(g) shouldBe 27
  }
}
