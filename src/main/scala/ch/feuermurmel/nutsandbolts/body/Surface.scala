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
  private def upTo(fn: Parameter => Double) =
    Surface({ p =>
      val end = fn(p)

      if (end > Double.NegativeInfinity)
        Ray(Double.NegativeInfinity, end)
      else
        Ray.empty
    })

  val empty = Surface(_ => Ray.empty)

  def intersection(surfaces: Seq[Surface]) =
    surfaces.reduceOption(_ & _).getOrElse(!Surface.empty)

  def union(surfaces: Seq[Surface]) =
    surfaces.reduceOption(_ | _).getOrElse(Surface.empty)

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

  // TODO: Move to CylindricalBody.
  def cone(z0: Double, r0: Double, slope: Double) =
    upTo(p => r0 + (p.z - z0) * slope)

  // TODO: Move to CylindricalBody.
  def coneSegment(z1: Double, z2: Double, r1: Double, r2: Double) =
    cone(z1, r1, (r2 - r1) / (z2 - z1))

  // TODO: Move to CylindricalBody.
  def cylinder(r: Double) =
    cone(0, r, 0)

  // TODO: Move to CylindricalBody.
  def sphere(r: Double) =
    upTo(p => sqrt(r * r - p.z * p.z))

  // TODO: Move to CylindricalBody.
  def plane(distance: Double) =
    upTo({ p =>
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

  // TODO: Move to CylindricalBody.
  def regularPolygon(sides: Int, innerRadius: Double) =
    (0 until sides).map(i => plane(innerRadius).rotate(tau * i / sides)).reduce(_ & _)

  def reverseSurface(surface: Surface) =
    Surface(p => surface(p.z, -p.c))
}
