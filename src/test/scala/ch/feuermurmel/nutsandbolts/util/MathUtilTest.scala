package ch.feuermurmel.nutsandbolts.util

import java.lang.Math.PI

import ch.feuermurmel.nutsandbolts.util.MathUtil.{mod, tau}
import org.scalatest.FreeSpec

class MathUtilTest extends FreeSpec {
  // Basically a test that testing works.
  "Tau is larger than Pi" in {
    tau > PI
  }

  def assertModResult(expected: Double)(x: Double, m: Double) = {
    val a = mod(x, m)

    if (m > 0)
      assert(0 <= a && a < m)
    else
      assert(m < a && a <= 0)

    assert(expected - 1e-6 < a && a < expected + 1e-6)
  }

  "mod(x, m) returns the correct result" in {
    assertModResult(0)(0, 5)
    assertModResult(1)(1, 5)
    assertModResult(1)(11, 5)

    assertModResult(0.25)(0.75, 0.5)

    assertModResult(2)(-3, 5)
    assertModResult(0)(10, 5)
    assertModResult(0)(-10, 5)
    assertModResult(-2)(3, -5)
  }

  "mod(x, m) only returns values between 0 and m" in {
    // Failing test case found during program runtime.
    assertModResult(0)(7.681973777933156, 2.560657925977719)
  }
}
