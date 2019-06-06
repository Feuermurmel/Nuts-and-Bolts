package ch.feuermurmel.nutsandbolts.body

import org.scalatest.FreeSpec

class RayTest extends FreeSpec {
  import Double.{PositiveInfinity => inf}

  private def r(start: Double, end: Double) =
    Ray(Seq(Interval(start, end)))

  private def r(start1: Double, end1: Double, start2: Double, end2: Double) =
    Ray(Seq(Interval(start1, end1), Interval(start2, end2)))

  "union operation works" in {
    assertResult(r(0, 1, 2, 3))(r(0, 1) | r(2, 3))
    assertResult(r(0, 2))(r(0, 1) | r(1, 2))
  }

  "intersection operation works" in {
    assertResult(Ray.empty)(r(0, 1) & r(2, 3))
    assertResult(Ray.empty)(r(0, 1) & r(1, 2))
    assertResult(r(1, 2))(r(0, 2) & r(1, 3))
  }

  "invert operation works" in {
    assertResult(Ray.empty)(!r(-inf, inf))
    assertResult(r(-inf, inf))(!Ray.empty)
    assertResult(r(-inf, 0, 1, inf))(!r(0, 1))
  }

  "shift operation works" in {
    assertResult(r(1, 2))(r(0, 1).shift(1))
  }

  "shift operation correctly handles overflows" in {
    assertResult(Ray.empty)(r(1e308, inf).shift(1e308))
  }

  "shift operation correctly merges intervals and holes" in {
    assertResult(Ray.empty)(r(0, 1).shift(1e53))
    assertResult(r(-inf, inf))((r(-inf, 0) | r(1, inf)).shift(1e53))
  }
}
