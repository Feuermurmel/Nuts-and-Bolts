package util

object MathUtil {
  def tau = Math.PI * 2

  def mod(x: Double, modulus: Double) = x - (x / modulus).floor * modulus
}
