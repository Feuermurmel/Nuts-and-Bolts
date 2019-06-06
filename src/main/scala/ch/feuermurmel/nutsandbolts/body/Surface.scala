package ch.feuermurmel.nutsandbolts.body

import java.lang.Math.{cos, sqrt}

import ch.feuermurmel.nutsandbolts.util.MathUtil.{mod, tau}

case class Surface(fn: Parameter => Ray) {
  def apply(coord: Parameter): Ray = fn(coord)

  def apply(z: Double, c: Double): Ray = this(Parameter(z, c))

  def rotate(aOffset: Double) = Surface(p => this(p.z, p.c - aOffset))

  def shift(zOffset: Double) = Surface(p => this(p.z - zOffset, p.c))

  def grow(rOffset: Double) = map(_.shift(rOffset))

  def unary_! = map(!_)

  def |(other: Surface) = zip(other, _ | _)

  def &(other: Surface) = zip(other, _ & _)

  def /(other: Surface) = zip(other, _ / _)

  private def map(fn: Ray => Ray) = Surface(p => fn(this(p)))

  private def zip(other: Surface, fn: (Ray, Ray) => Ray) =
    Surface(p => fn(this(p), other(p)))
}

object Surface {
  def fromZero(fn: Parameter => Double) =
    Surface({ p =>
      val end = fn(p)

      if (end > 0)
        Ray(Seq(Interval(0, end)))
      else
        Ray.empty
    })

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
    fromZero(p => r0 + (p.z - z0) * slope)

  def coneSegment(z1: Double, z2: Double, r1: Double, r2: Double) =
    cone(z1, r1, (r2 - r1) / (z2 - z1))

  def cylinder(r: Double) =
    cone(0, r, 0)

  def sphere(r: Double) =
    fromZero(p => sqrt(r * r - p.z * p.z))

  def plane(distance: Double) =
    fromZero({ p =>
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
