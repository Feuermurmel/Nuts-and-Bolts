import java.lang.Math.sqrt

import Surface.{cone, coneSegment, plane}
import util.MathUtil.{mod, tau}

object NutsAndBolts {
  /**
    * Operation applied to the faces of a surface (planes perpendicular to the z-axes by which a sliced surface is limited).
    */
  trait Facing {
    def apply(surface: Surface): Surface
  }

  object Facing {
    def none: Facing = identity(_)

    def outsideChamfer(r: Double): Facing = _ & cone(0, r, 1)
    def insideChamfer(r: Double): Facing = _ | cone(0, r, -1)
  }

  trait BoltHead {
    def surface: Surface
    def cutoffFacing: Facing
    def nominalHeight: Double
  }

  trait Thread {
    def outwardSurface: Surface
    def inwardSurface: Surface

    def maleCutoffFacing: Facing
    def maleGraftFacing: Facing

    def femaleCutoffFacing: Facing
    def femaleGraftFacing: Facing
  }

  trait Rod {
    def part(threadLength: Double): Part
  }

  object Rod {
    def simpleBolt(thread: Thread, head: BoltHead) =
      new Rod {
        override def part(threadLength: Double) = {
          Part.withoutHole(
            sliceWithFacings(
              head.surface,
              head.nominalHeight,
              head.cutoffFacing,
              head.cutoffFacing),
            sliceWithFacings(
              thread.outwardSurface,
              threadLength,
              thread.maleGraftFacing,
              thread.maleCutoffFacing))
        }
      }

    def threadedRod(thread: Thread) =
      new Rod {
        override def part(threadLength: Double) = {
          Part.withoutHole(
            sliceWithFacings(
              thread.outwardSurface,
              threadLength,
              thread.maleCutoffFacing,
              thread.maleCutoffFacing))
        }
      }

    def nut(thread: Thread, head: BoltHead) =
      Part.withHole(
        sliceWithFacings(
          head.surface,
          head.nominalHeight,
          head.cutoffFacing,
          head.cutoffFacing),
        sliceWithFacings(
          thread.inwardSurface,
          head.nominalHeight,
          thread.femaleCutoffFacing,
          thread.femaleCutoffFacing).invert)
  }

  def sliceWithFacings(surface: Surface, height: Double, bottomFacing: Facing, topFacing: Facing) = {
    def flipPart(surface: Surface) = Surface(p => surface(height - p.z, p.c))

    flipPart(topFacing(flipPart(bottomFacing(surface)))).slice(height)
  }

  def repeatedSurface(surface: Surface, zMax: Double) =
    Surface(p => surface(mod(p.z, zMax), p.c))

  def skewedSurface(surface: Surface, zShift: Double) =
    Surface(p => surface(p.z - p.c / tau * zShift, p.c))

  def regularPolygon(sides: Int, innerRadius: Double) =
    (0 until sides).map(i => plane(innerRadius).rotate(tau * i / sides)).reduce(_ & _)

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
