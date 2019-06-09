package ch.feuermurmel.nutsandbolts.util

import java.lang.Math.{PI, sqrt}

object MathUtil {
  val tau = PI * 2

  val phi = (1 + sqrt(5)) / 2

  val goldenAngle = tau / (1 + phi)

  def mod(x: Double, modulus: Double) = {
    val a = x / modulus

    (a - a.floor) * modulus
  }

  def isFinite(x: Double): Boolean = !x.isInfinite && !x.isNaN

  def isFinite(x: Float): Boolean = !x.isInfinite && !x.isNaN

  def quantile(values: Seq[Double], q: Double) =
    values.sorted.apply(((values.size - 1) * q).toInt)

  def closedRange(start: Double, end: Double, step: Double) =
    (0 to ((end - start) / step).toInt).map(start + step * _)
}
