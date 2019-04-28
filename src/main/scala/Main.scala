import java.lang.Math.sqrt
import java.nio.file.Paths

import Surface.{cone, coneSegment, plane}
import util.MathUtil
import util.MathUtil.tau

object Main extends App {
  def repeatedSurface(surface: Surface, zMax: Double) =
    Surface(p => surface(MathUtil.mod(p.z, zMax), p.c))

  def skewedSurface(surface: Surface, zShift: Double) =
    Surface(p => surface(p.z - p.c / tau * zShift, p.c))

  def regularPolygon(sides: Int, innerRadius: Double) =
    (0 until sides).map(i => plane(innerRadius).rotate(tau * i / sides)).reduce(_ & _)

  def isoThread(majorDiameter: Double, pitch: Double, includedAngle: Double = tau / 6) = {
    val h = pitch * sqrt(3) / 2

    val majorRadius = majorDiameter / 2
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

    val surface = Surface.piecewise((piece1, z1), (piece2, z2), (piece3, z3), (piece4, z4))

    skewedSurface(repeatedSurface(surface, pitch), pitch)
  }

  case class Screw(size: Double, pitch: Double, headWidth: Double) {
    val radius = size / 2
    val headHeight = headWidth / 2
    val tolerance = 0.2

    val nutChamfer = headWidth * 0.03
    val nutInnerChamfer = size * 0.03

    // Chamfer used to taper the end of the screw into a circle on tip of the screw and near the head.
    val threadChamfer = pitch

    private def headSurface = {
      val headInnerRadius = headWidth / 2

      (
        regularPolygon(6, headInnerRadius)
          & cone(0, headInnerRadius, 1).grow(-nutChamfer)
          & cone(headHeight, headInnerRadius, -1).grow(-nutChamfer))
    }

    private def threadSurface(offset: Double) =
      isoThread(size, pitch).grow(offset)

    def screw(length: Double) = {
      val thread = (
        threadSurface(-tolerance / 2)
          & cone(length, radius, -1).grow(-threadChamfer))

      Part.withoutHole(
        headSurface.slice(headHeight),
        (thread | cone(0, radius, -1)).slice(length))
    }

    def nut = {
      val thread = (
        threadSurface(tolerance / 2)
          | cone(0, radius, -1).grow(nutInnerChamfer)
          | cone(headHeight, radius, 1).grow(nutInnerChamfer))

      Part.withHole(
        headSurface.slice(headHeight),
        thread.slice(headHeight).invert)
    }
  }

  case class Washer(innerDiameter: Double, outerDiameter: Double, thickness: Double) {
    val tolerance = 0.2

    def surface = Part.withHole(
      Surface.cylinder(outerDiameter / 2).slice(thickness),
      Surface.cylinder(innerDiameter / 2 + tolerance / 2).slice(thickness).invert)
  }

  val outputPath = Paths.get("output")

  def writePart(part: Part, name: String): Unit = {
    val path = outputPath.resolve(s"$name.stl")

    println(s"Writing $path ...")

    val zResolution = 0.1
    val aPoints = 6 * 30

    part.toPolyhedron(zResolution, tau / aPoints).writeToSTLFile(path)
  }

  def writeNutAndScrew(name: String, screw: Screw, screwLength: Double): Unit = {
    writePart(screw.nut, s"$name-nut")
    writePart(screw.screw(screwLength), s"$name-screw")
  }

  def writeWasher(name: String, washer: Washer): Unit =
    writePart(washer.surface, s"$name-washer")

  def main(): Unit = {
    writeNutAndScrew("M2", Screw(2, 0.4, 4), 8)
    writeNutAndScrew("M3", Screw(3, 0.5, 5.5), 9)
    writeNutAndScrew("M4", Screw(4, 0.7, 7), 10)
    writeNutAndScrew("M5", Screw(5, 0.8, 8), 11)
    writeNutAndScrew("M6", Screw(6, 1, 10), 12)
    writeNutAndScrew("M8", Screw(8, 1.25, 13), 16)

    writeNutAndScrew("M10", Screw(10, 1.5, 16), 20)
    writeNutAndScrew("M20", Screw(20, 2.5, 30), 40)
    writeNutAndScrew("M24", Screw(24, 3, 36), 50)

    writeWasher("M8", Washer(8.4, 16, 1.6))
    writeWasher("M10", Washer(10.5, 20, 2))
  }

  main()
}
