package ch.feuermurmel.nutsandbolts.util

object MathUtil {
  val tau = Math.PI * 2

  def mod(x: Double, modulus: Double) = x - (x / modulus).floor * modulus
}
