package util

import java.lang.Math.{max, round}

object MathUtil {
  def tau = Math.PI * 2

  def mod(x: Double, modulus: Double) = x - (x / modulus).floor * modulus

  private def rangeWithResolution(start: Double, end: Double, resolution: Double, inclusive: Boolean): Seq[Double] = {
    val size = end - start
    val steps = max(2, round(size / resolution).toInt)
    val range = if (inclusive) 0 to steps else 0 until steps

    range.map(start + _ * size / steps)
  }

  def rangeWithResolution(start: Double, end: Double, resolution: Double): Seq[Double] = rangeWithResolution(start, end, resolution, inclusive = false)

  def inclusiveRangeWithResolution(start: Double, end: Double, resolution: Double) = rangeWithResolution(start, end, resolution, inclusive = true)
}
