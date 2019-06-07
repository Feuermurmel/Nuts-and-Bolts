package ch.feuermurmel.nutsandbolts.body

import ch.feuermurmel.nutsandbolts.util.MathUtil.tau
import org.scalatest.FreeSpec

class CylindricalBodyTest extends FreeSpec {
  import Double.{PositiveInfinity => inf}

  def z(end: Double) = Ray(0, end)

  "halfSpace() with positive distance works" in {
    val s = CylindricalBody.halfSpace(0, 1)

    // Base case.
    assertResult(z(1))(s(0, 0))

    // Rotated half-plane.
    assertResult(z(1))(CylindricalBody.halfSpace(tau / 4, 1)(0, tau / 4))

    // Intersect border at odd angle.
    val rayEnd = s(0, tau / 6).hull.get.end
    assert(1.9 < rayEnd && rayEnd < 2.1)

    // Cases where ray should not intersect the border.
    assertResult(z(inf))(s(0, tau / 4 + 1e-10))
    assertResult(z(inf))(s(0, -tau / 4 - 1e-10))
    assertResult(z(inf))(s(0, tau / 2))

    // Different Distance.
    assertResult(z(2))(CylindricalBody.halfSpace(0, 2)(0, 0))
  }

  "halfSpace() with negative distance works" in {
    val s = CylindricalBody.halfSpace(0, -1)

    // Base cases.
    assertResult(Ray.empty)(s(0, 0))
    assertResult(Ray(1, inf))(s(0, tau / 2))

    // Rotated half-plane.
    assertResult(Ray(1, inf))(CylindricalBody.halfSpace(tau / 4, -1)(0, tau * 3 / 4))

    // Cases where ray should not intersect the border.
    assertResult(Ray.empty)(s(0, tau / 4 - 1e-10))
    assertResult(Ray.empty)(s(0, -tau / 4 + 1e-10))
  }

  "halfSpace() through origin works" in {
    val s = CylindricalBody.halfSpace(0, 0)

    // Base cases.
    assertResult(Ray.empty)(s(0, 0))
    assertResult(z(inf))(s(0, tau / 2))

    // Other directions.
    assertResult(Ray.empty)(s(0, tau / 4 - 1e-10))
    assertResult(z(inf))(s(0, tau / 4 + 1e-10))
  }
}
