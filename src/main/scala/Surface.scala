trait Surface {
  def apply(z: Double, a: Double): Double

  def rotate(aOffset: Double) = Surface((z, a) => this(z, a - aOffset))

  def shift(zOffset: Double) = Surface((z, a) => this(z - zOffset, a))

  def radialShift(rOffset: Double) = Surface((z, a) => this(z, a) + rOffset)
}

object Surface {
  def apply(fn: (Double, Double) => Double): Surface =
    new Surface {
      override def apply(z: Double, a: Double): Double = fn(z, a)
    }
}
