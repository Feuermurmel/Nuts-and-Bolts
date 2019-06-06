import java.lang.Math.sqrt

import ch.feuermurmel.nutsandbolts.body.Surface
import ch.feuermurmel.nutsandbolts.body.Surface.{coneSegment, regularPolygon, repeatedSurface, skewedSurface}
import ch.feuermurmel.nutsandbolts.part.{BoltHead, Facing, Thread}

object ISO {
  def isoThread(size: Double, pitch: Double) = {
    val tolerance = 0.2

    // Chamfer used to taper the end of the screw into a circle on tip of the screw and near the head.
    val threadChamfer = pitch

    val nutThreadChamfer = size * 0.03

    val h = pitch * sqrt(3) / 2

    val majorRadius = size / 2
    val minorRadius = majorRadius - h * 5 / 8

    val z0 = 0
    val z1 = z0 + pitch / 4
    val z2 = z1 + pitch * 5 / 16
    val z3 = z2 + pitch / 8
    val z4 = z3 + pitch * 5 / 16

    val piece1 = coneSegment(z0, z1, minorRadius, minorRadius)
    val piece2 = coneSegment(z1, z2, minorRadius, majorRadius)
    val piece3 = coneSegment(z2, z3, majorRadius, majorRadius)
    val piece4 = coneSegment(z3, z4, majorRadius, minorRadius)

    val threadShape = Surface.piecewise(
      (piece1, z1),
      (piece2, z2),
      (piece3, z3),
      (piece4, z4))

    val threadSurface = skewedSurface(repeatedSurface(threadShape, pitch), pitch)

    new Thread {
      override def outwardSurface = threadSurface.grow(-tolerance / 2)
      override def inwardSurface = threadSurface.grow(tolerance / 2)
      override def maleCutoffFacing = Facing.outsideChamfer(majorRadius - threadChamfer)
      override def maleGraftFacing = Facing.insideChamfer(majorRadius)
      override def femaleCutoffFacing = Facing.insideChamfer(majorRadius + nutThreadChamfer)
      override def femaleGraftFacing = Facing.none
    }
  }

  def isoBoltHead(size: Double) = {
    val innerRadius = size / 2
    val chamferRadius = innerRadius - size * 0.03

    new BoltHead {
      override def surface = regularPolygon(6, innerRadius)
      override def cutoffFacing = Facing.outsideChamfer(chamferRadius)
      override def nominalHeight = size / 2
    }
  }
}
