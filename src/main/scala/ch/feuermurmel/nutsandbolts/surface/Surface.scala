package ch.feuermurmel.nutsandbolts.surface

import java.lang.Math.{cos, max, min, sqrt}

import ch.feuermurmel.nutsandbolts.util.MathUtil.{mod, tau}

case class Surface(fn: Surface.Parameter => Double) {
  def apply(coord: Surface.Parameter): Double = fn(coord)

  def apply(z: Double, c: Double): Double = this(Surface.Parameter(z, c))

  def rotate(aOffset: Double) = Surface(p => this(p.z, p.c - aOffset))

  def shift(zOffset: Double) = Surface(p => this(p.z - zOffset, p.c))

  def grow(rOffset: Double) = Surface(p => this(p) + rOffset)

  def |(other: Surface) = Surface(p => max(this(p), other(p)))

  def &(other: Surface) = Surface(p => min(this(p), other(p)))

  def slice(zStart: Double, zEnd: Double): SurfaceSlice =
    SurfaceSlice(shift(-zStart), zEnd - zStart, SurfaceSlice.Orientation.Outward)

  def slice(zEnd: Double): SurfaceSlice = slice(0, zEnd)
}

object Surface {
  /**
    * Represents a point in the coordinate system over which surfaces are parameterized.
    *
    * @param z The offset along the z-axis.
    * @param c The rotation around the z-axis.
    */
  case class Parameter(z: Double, c: Double)

  def select(getSurface: Parameter => Surface) =
    Surface(p => getSurface(p)(p))

  // TODO: Rewrite this comment.
  /**
    * Surface which starts at z == -inf with the first surface up to the first z height followed by surface(i) up to z(i). After z(n), switches back to the first surface.
    */
  def piecewise(pieces: (Surface, Double)*) =
    select({ p =>
      pieces.collectFirst({ case (s, z) if p.z < z => s }).getOrElse(pieces.head._1)
    })

  def cone(z0: Double, r0: Double, slope: Double) =
    Surface(p => r0 + (p.z - z0) * slope)

  def coneSegment(z1: Double, z2: Double, r1: Double, r2: Double) =
    cone(z1, r1, (r2 - r1) / (z2 - z1))

  def cylinder(r: Double) =
    cone(0, r, 0)

  def sphere(r: Double) =
    Surface(p => sqrt(r * r - p.z * p.z))

  def plane(distance: Double) =
    Surface({ p =>
      val c = cos(p.c)

      if (c > 0)
        distance / c
      else
        Double.PositiveInfinity
    })

  def repeatedSurface(surface: Surface, zMax: Double) =
    Surface(p => surface(mod(p.z, zMax), p.c))

  def skewedSurface(surface: Surface, zShift: Double) =
    Surface(p => surface(p.z - p.c / tau * zShift, p.c))

  def regularPolygon(sides: Int, innerRadius: Double) =
    (0 until sides).map(i => plane(innerRadius).rotate(tau * i / sides)).reduce(_ & _)

  def reverseSurface(surface: Surface) =
    Surface(p => surface(p.z, -p.c))
}
