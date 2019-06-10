package ch.feuermurmel.nutsandbolts.knurl

import ch.feuermurmel.nutsandbolts.body.Surface.{coneSegment, repeatedSurface, reverseSurface, skewedSurface}
import ch.feuermurmel.nutsandbolts.body.{Parameter, Surface}

case class Knurl(pattern: KnurlPattern, shape: KnurlShape, mode: KnurlMode) {
  val pitch = pattern.advance / pattern.ridges

  val depth = {
    val ridgeDistance = pattern.distance / pattern.ridges
    val flatRatio = 1 - shape.majorFlatRatio - shape.minorFlatRatio

    shape.slope * flatRatio * ridgeDistance / 2
  }

  private def getSurface = {
    val z0 = -shape.minorFlatRatio / 2 * pitch
    val z1 = shape.minorFlatRatio / 2 * pitch
    val z2 = (1 - shape.majorFlatRatio) / 2 * pitch
    val z3 = (1 + shape.majorFlatRatio) / 2 * pitch
    val z4 = (1 - shape.minorFlatRatio / 2) * pitch

    val piece1 = coneSegment(z0, z1, -depth, -depth)
    val piece2 = coneSegment(z1, z2, -depth, 0)
    val piece3 = coneSegment(z2, z3, 0, 0)
    val piece4 = coneSegment(z3, z4, 0, -depth)

    // This surface actually has a distance from the Z-axis that is negative in most places.
    skewedSurface(
      repeatedSurface(
        Surface.piecewise((piece1, z1), (piece2, z2), (piece3, z3), (piece4, z4)),
        pitch),
      pattern.advance)
  }

  private val knurlSurface = {
    val right = getSurface
    val left = reverseSurface(getSurface)

    mode match {
      case KnurlMode.Left => left
      case KnurlMode.Right => right
      case KnurlMode.BothAdditive => left | right
      case KnurlMode.BothSubtractive => left & right
    }
  }

  // TODO: Maybe just knurl the outermost surface here?
  // TODO: Represent knurl surface by type that does not allow arbitrary rays
  def applyToSurface(surface: Surface) =
    Surface(p => surface(p).shift(knurlSurface(p).hull.get.end))
}
