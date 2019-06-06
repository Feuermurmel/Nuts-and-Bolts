package ch.feuermurmel.nutsandbolts.body

import ch.feuermurmel.nutsandbolts.polyhedron.Point

class BodyTest extends PolyhedronSpec {
  implicit class StringContextRayOps(val sc: StringContext) {
    def r(args: Any*): Ray = {
      val Seq(part) = sc.parts

      part.zipWithIndex.map({
        case ('1', index) => Ray(index, index + 1)
        case ('0', _) => Ray.empty
        case _ => ???
      }).fold(Ray.empty)(_ | _)
    }
  }

  def defaultPointOnRay(index: (Int, Int), value: Double) =
    Point(index._1, index._2, value)

  def constructPolyhedron(r00: Ray, r01: Ray, r11: Ray, r10: Ray = Ray.empty, sizeI: Int = 2, sizeJ: Int = 2, wrapI: Boolean = false, wrapJ: Boolean = false, pointOnRay: ((Int, Int), Double) => Point = defaultPointOnRay) = {
    def combineIndex(i: Int, j: Int) = (i, j)

    // We have the lower left under control, the rest is just extended from the right or top edge, which is enough for the simple test cases.
    def rayAtIndex(index: (Int, Int)) =
      index match {
        case (0, 0) => r00
        case (0, _) => r01
        case (_, 0) => r10
        case (_, _) => r11
        case _ => Ray.empty
      }

    Body.constructPolyhedron[(Int, Int)](sizeI, sizeJ, combineIndex, rayAtIndex, pointOnRay, wrapI, wrapJ)
  }

  "empty cells" in {
    // Lots of cases which all shouldn't generate any faces.
    assertPolyhedronProperties(constructPolyhedron(r"", r"", r""), 0)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"", r""), 0)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r""), 0)
    assertPolyhedronProperties(constructPolyhedron(r"11", r"011", r""), 0)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"001"), 0)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"001", r"1", r"001"), 0)
  }

  "simple prisms" in {
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"1"), 8)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"1", r"1"), 12)
  }

  "skewed prism" in {
    assertPolyhedronProperties(constructPolyhedron(r"011", r"11", r"0011", r"00011"), 12)
  }

  "warped coordinate systems" in {
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"1", r"1", sizeI = 3, wrapI = true), 24)
    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"1", r"1", sizeJ = 3, wrapJ = true), 24)
  }

  "non-linear coordinate system" in {
    def pointOnRay(index: (Int, Int), value: Double) =
      Point(index._1 * value, index._2, value)

    assertPolyhedronProperties(constructPolyhedron(r"1", r"1", r"1", r"1", pointOnRay = pointOnRay), 8)
  }
}
