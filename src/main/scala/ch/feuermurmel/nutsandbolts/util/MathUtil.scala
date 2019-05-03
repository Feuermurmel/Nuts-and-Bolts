package ch.feuermurmel.nutsandbolts.util

import java.lang.Math.{PI, sqrt}

object MathUtil {
  val tau = PI * 2

  val phi = (1 + sqrt(5)) / 2

  def mod(x: Double, modulus: Double) = x - (x / modulus).floor * modulus

  def isFinite(x: Double): Boolean = !x.isInfinite && !x.isNaN

  def isFinite(x: Float): Boolean = !x.isInfinite && !x.isNaN
}
