package ch.feuermurmel.nutsandbolts.body

import org.scalatest.FreeSpec

class IntervalTest extends FreeSpec {
  import Double.{NaN, PositiveInfinity => inf}

  "interval with NaN ends cannot be constructed" in {
    assertThrows[IllegalArgumentException](Interval(NaN, 0))
    assertThrows[IllegalArgumentException](Interval(0, NaN))
  }

  "interval with misaligned ends cannot be constructed" in {
    assertThrows[IllegalArgumentException](Interval(1, 0))
    assertThrows[IllegalArgumentException](Interval(inf, 0))
    assertThrows[IllegalArgumentException](Interval(0, -inf))
  }

  "empty interval cannot be constructed" in {
    assertThrows[IllegalArgumentException](Interval(1, 1))
  }

  "intersection of intervals works" in {
    assertResult(Some(Interval(2, 3)))(Interval(-inf, inf) & Interval(2, 3))
    assertResult(Some(Interval(1, 2)))(Interval(0, 2) & Interval(1, 3))
  }

  "intersection of disjoint intervals is empty" in {
    assertResult(None)(Interval(0, 1) & Interval(2, 3))
    assertResult(None)(Interval(0, 1) & Interval(1, 2))
  }
}
