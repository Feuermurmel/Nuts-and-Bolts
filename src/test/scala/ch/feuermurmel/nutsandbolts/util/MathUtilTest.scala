package ch.feuermurmel.nutsandbolts.util

import java.lang.Math.PI

import ch.feuermurmel.nutsandbolts.util.MathUtil.tau
import org.scalatest.FreeSpec

class MathUtilTest extends FreeSpec {
  // Basically a test that testing works.
  "Tau is larger than Pi" in {
    tau > PI
  }
}
