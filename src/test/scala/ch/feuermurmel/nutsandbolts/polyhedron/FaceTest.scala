package ch.feuermurmel.nutsandbolts.polyhedron

import org.scalatest.FreeSpec

class FaceTest extends FreeSpec {
  "calculation of normals works" in {
    assertResult(
      Point(0, 0, 1))(
      Face(Point(0, 0, 0), Point(1, 0, 0), Point(0, 1, 0)).normal)

    assertResult(
      Point(0, 0, -1))(
      Face(Point(0, 0, 5), Point(1, 2, 5), Point(2, 1, 5)).normal)
  }

  "polygon with zero area has zero normal" in {
    assertResult(
      Point(0, 0, 0))(
      Face(Point(0, 0, 0), Point(0, 0, 0), Point(0, 0, 0)).normal)

    assertResult(
      Point(0, 0, 0))(
      Face(Point(0, 0, 0), Point(2, 2, 2), Point(4, 4, 4)).normal)
  }
}
