import java.lang.Math.{max, min}

trait Surface {
  def apply(z: Double, a: Double): Double

  def rotate(aOffset: Double) = Surface((z, a) => this(z, a - aOffset))

  def shift(zOffset: Double) = Surface((z, a) => this(z - zOffset, a))

  def grow(rOffset: Double) = Surface((z, a) => this(z, a) + rOffset)

  def |(other: Surface) = Surface((z, a) => max(this(z, a), other(z, a)))

  def &(other: Surface) = Surface((z, a) => min(this(z, a), other(z, a)))
}

object Surface {
  def apply(fn: (Double, Double) => Double): Surface = (z, a) => fn(z, a)

  def select(getSurface: (Double, Double) => Surface) =
    Surface((z, a) => getSurface(z, a)(z, a))

  def cone(z0: Double, r0: Double, slope: Double) =
    Surface((z, _) => r0 + (z - z0) * slope)

  def plane(distance: Double) =
    Surface({ (_, a) =>
      val c = Math.cos(a)

      if (c > 0)
        distance / c
      else
        Double.PositiveInfinity
    })

  def coneSegment(z1: Double, z2: Double, r1: Double, r2: Double) =
    cone(z1, r1, (r2 - r1) / (z2 - z1))
}
